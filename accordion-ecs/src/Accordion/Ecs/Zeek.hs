{-# language BangPatterns #-}
{-# language NamedFieldPuns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}

module Accordion.Ecs.Zeek
  ( zeekToEcs
  ) where

import Accordion.Ecs (Attributes(Attributes))
import Data.Chunks (Chunks)

import Control.Monad.ST (ST,runST)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))
import Data.Monoid (All(All))
import Data.Primitive (Array,SmallArray)
import Data.Array.Indexed (Vector)
import Data.Array.Bool (MutableBoolVector)
import Data.Word (Word16)
import Data.STRef (STRef,writeSTRef,readSTRef,newSTRef)
import Data.Index (Index(Index))
import Data.Word (Word64)
import Accordion.Ecs (column)
import Accordion.Types (Rec(RecCons,RecNil))
import Chronos (Datetime)

import qualified Chronos
import qualified Accordion.Ecs as Ecs
import qualified Data.Index as Index
import qualified Data.Arithmetic.Types as Arithmetic
import qualified World.Bool as Bool
import qualified Data.Array.Indexed as V
import qualified Zeek.Json as Zeek
import qualified GHC.TypeNats as GHC

import qualified Deferred.Word16 as Word16
import qualified Deferred.Word64 as Word64

-- | This fails if the timestamp is not present for every log.
zeekToEcs ::
     Array (Chunks Zeek.Attribute)
  -> Maybe Attributes
zeekToEcs xs = runST
  ( V.with xs
    (\vec -> do
      w <- initializeWorking
      let !n = V.length vec
      Index.ascendM (\ix -> steps vec w n ix) n
      freezeA n w
    )
  )

initializeWorking :: ST s (Working n s)
initializeWorking = do
  timestamp <- newSTRef Word64.Absent
  dns_question_class <- newSTRef Word16.Absent
  dns_question_type <- newSTRef Word16.Absent
  source_port <- newSTRef Word16.Absent
  pure Working
    { timestamp, dns_question_class, source_port
    , dns_question_type
    }

-- dns.question.class_number
-- This one is a little different since it assumes
-- that the predecessor is the empty attributes.
freezeA ::
     Arithmetic.Nat n
  -> Working n s
  -> ST s (Maybe Attributes)
freezeA !n !w = do
  x <- readSTRef (dns_question_class w)
  Word16.freeze x >>= \case
    Nothing -> freezeB n w Ecs.empty
    Just (vs,bs) -> freezeB n w
      (Ecs.nestedColumn
        (RecCons Ecs.dns $ RecCons Ecs.question $ RecNil)
        Ecs.classNumber
        bs vs
      )

-- source.port
freezeB :: 
     Arithmetic.Nat n
  -> Working n s
  -> Ecs.Optionals n rs
  -> ST s (Maybe Attributes)
freezeB !n !w !opt = do
  x <- readSTRef (source_port w)
  Word16.freeze x >>= \case
    Nothing -> freezeC n w opt
    Just (vs,bs) -> freezeC n w $ Ecs.union opt
      (Ecs.nestedColumn
        (RecCons Ecs.source $ RecNil)
        Ecs.port
        bs vs
      )

-- dns.question.type_number
freezeC :: 
     Arithmetic.Nat n
  -> Working n s
  -> Ecs.Optionals n rs
  -> ST s (Maybe Attributes)
freezeC !n !w !opt = do
  x <- readSTRef (dns_question_type w)
  Word16.freeze x >>= \case
    Nothing -> freezeD n w opt
    Just (vs,bs) -> freezeD n w $ Ecs.union opt
      (Ecs.nestedColumn
        (RecCons Ecs.dns $ RecCons Ecs.question $ RecNil)
        Ecs.typeNumber
        bs vs
      )

freezeD ::
     Arithmetic.Nat n
  -> Working n s
  -> Ecs.Optionals n rs
  -> ST s (Maybe Attributes)
freezeD n w opt = do
  x <- readSTRef (timestamp w)
  Word64.freeze x >>= \case
    Nothing -> pure Nothing
    Just (vs,bs) -> do
      let Identity (All valid) = Index.ascendM
            (\(Index lt off) -> Identity (All (Bool.index lt bs off)))
            n
      case valid of
        True -> pure (Just (Attributes n vs opt))
        False -> pure Nothing

data Working (n :: GHC.Nat) s = Working
  { timestamp :: !(STRef s (Word64.Deferred s n))
  , dns_question_class :: !(STRef s (Word16.Deferred s n))
  , dns_question_type :: !(STRef s (Word16.Deferred s n))
  , source_port :: !(STRef s (Word16.Deferred s n))
  }

steps ::
     Vector n (Chunks Zeek.Attribute)
  -> Working n s
  -> Arithmetic.Nat n
  -> Index n -- offset
  -> ST s ()
steps !srcs !working !n !ix@(Index lt off) = traverse_
  (\src -> step src working n ix)
  (V.index lt srcs off)

step ::
     Zeek.Attribute -- source
  -> Working n s
  -> Arithmetic.Nat n
  -> Index n -- offset
  -> ST s ()
step !src !working !n !ix@(Index lt off) = case src of
  Zeek.Word16Attribute attr val -> case attr of
    Zeek.QueryClass -> Word16.write (dns_question_class working) n ix val
    Zeek.QueryType -> Word16.write (dns_question_type working) n ix val
    Zeek.IdOrigPort -> Word16.write (source_port working) n ix val
    _ -> pure ()
  Zeek.TimeAttribute attr val -> case attr of
    Zeek.Timestamp -> Word64.write (timestamp working) n ix (fromIntegral (Chronos.getTime (Chronos.datetimeToTime val)))
    _ -> pure ()
  _ -> pure ()

