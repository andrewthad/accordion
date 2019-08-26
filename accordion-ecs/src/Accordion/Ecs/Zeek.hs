{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}

module Accordion.Ecs.Zeek
  ( zeekToEcs
  ) where

import Accordion.Ecs (Attributes(Attributes))

import Control.Monad.ST (ST,runST)
import Data.Monoid (All(All))
import Data.Primitive (Array)
import Data.Array.Indexed (Vector)
import Data.Array.Word16 (MutableWord16Vector)
import Data.Array.Word64 (MutableWord64Vector)
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
import qualified Data.Array.Word16 as Word16
import qualified Data.Array.Word64 as Word64
import qualified Data.Array.Bool as Bool
import qualified Data.Array.Indexed as V
import qualified Zeek.Json as Zeek
import qualified GHC.TypeNats as GHC

-- | This fails if the timestamp is not present for every log.
zeekToEcs ::
     Array Zeek.Attribute
  -> Maybe Attributes
zeekToEcs xs = runST
  ( V.with xs
    (\vec -> do
      w <- initializeWorking
      let !n = V.length vec
      Index.ascendM (\ix -> step vec w n ix) n
      freezeA n w
    )
  )

initializeWorking :: ST s (Working n s)
initializeWorking = Working
  <$> newSTRef MW64No
  <*> newSTRef MW16No

freezeA ::
     Arithmetic.Nat n
  -> Working n s
  -> ST s (Maybe Attributes)
freezeA !n !w = readSTRef (dns_question_class w) >>= \case
  MW16No -> freezeFinal Ecs.empty n w
  MW16Yes vs bs -> do
    bs' <- Bool.unsafeFreeze bs
    vs' <- Word16.unsafeFreeze vs
    freezeFinal
      (Ecs.nestedColumn
        (RecCons Ecs.dns $ RecCons Ecs.question $ RecNil)
        Ecs.classNumber
        bs' vs'
      ) n w

freezeFinal ::
     Ecs.Optionals n m
  -> Arithmetic.Nat n
  -> Working n s
  -> ST s (Maybe Attributes)
freezeFinal opt n w = readSTRef (timestamp w) >>= \case
  MW64No -> pure Nothing
  MW64Yes vs bs -> do
    All valid <- Index.ascendM
      (\(Index lt off) -> fmap All (Bool.read lt bs off))
      n
    case valid of
      True -> do
        vs' <- Word64.unsafeFreeze vs
        pure (Just (Attributes n vs' opt))
      False -> pure Nothing

data MW16 (n :: GHC.Nat) s
  = MW16Yes
      !(MutableWord16Vector s n)
      !(MutableBoolVector s n)
  | MW16No

data MW64 (n :: GHC.Nat) s
  = MW64Yes
      !(MutableWord64Vector s n)
      !(MutableBoolVector s n)
  | MW64No

data Working (n :: GHC.Nat) s = Working
  { timestamp :: !(STRef s (MW64 n s))
  , dns_question_class :: !(STRef s (MW16 n s))
  }
 
step ::
     Vector n Zeek.Attribute -- source
  -> Working n s
  -> Arithmetic.Nat n
  -> Index n -- offset
  -> ST s ()
step !src !working !n !ix@(Index lt off) = case V.index lt src off of
  Zeek.Word16Attribute attr val -> case attr of
    Zeek.QueryClass -> writeW16 (dns_question_class working) n ix val
  Zeek.TimeAttribute attr val -> case attr of
    Zeek.Timestamp -> writeTime (timestamp working) n ix val

writeTime ::
     STRef s (MW64 n s)
  -> Arithmetic.Nat n
  -> Index n
  -> Datetime
  -> ST s ()
writeTime !ref !n !ix@(Index lt off) !time = do
  (vs, bs) <- ensureW64 ref n
  Bool.write lt bs off True
  Word64.write lt vs off
    (fromIntegral (Chronos.getTime (Chronos.datetimeToTime time)))

writeW16 ::
     STRef s (MW16 n s)
  -> Arithmetic.Nat n
  -> Index n
  -> Word16
  -> ST s ()
writeW16 !ref !n !ix@(Index lt off) !v = do
  (vs, bs) <- ensureW16 ref n
  Bool.write lt bs off True
  Word16.write lt vs off v

ensureW16 ::
     STRef s (MW16 n s)
  -> Arithmetic.Nat n
  -> ST s (MutableWord16Vector s n, MutableBoolVector s n)
ensureW16 !ref !n = readSTRef ref >>= \case
  MW16Yes x y -> pure (x,y)
  MW16No -> do
    !vs <- Word16.initialize n (0 :: Word16)
    !bs <- Bool.initialize n False
    let !x = MW16Yes vs bs
    writeSTRef ref x
    pure (vs,bs)

ensureW64 ::
     STRef s (MW64 n s)
  -> Arithmetic.Nat n
  -> ST s (MutableWord64Vector s n, MutableBoolVector s n)
ensureW64 !ref !n = readSTRef ref >>= \case
  MW64Yes x y -> pure (x,y)
  MW64No -> do
    !vs <- Word64.initialize n (0 :: Word64)
    !bs <- Bool.initialize n False
    let !x = MW64Yes vs bs
    writeSTRef ref x
    pure (vs,bs)
