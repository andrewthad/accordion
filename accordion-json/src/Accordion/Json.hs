{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Accordion.Json
  ( encode
  ) where

import Accordion.Base (Vectorized(..),Records(..))
import Data.Array.Indexed (MutableVector,Vector)
import Data.Primitive (MutableByteArray,ByteArray)
import Accordion.Types (ApConst2(..),Vec(..))
import Control.Monad.ST (ST,runST)
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import Data.Word (Word8)
import Data.Char (ord)
import Data.Bytes.Types (Bytes(..))
import Control.Monad (when)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Kind (Type)
import Accordion.Json.Types (Encode(..))

import qualified Accordion.Base as Base
import qualified Accordion.Base.Signature as B
import qualified Accordion.Json.Signature as J
import qualified Accordion.Types as A
import qualified Accordion.World as A
import qualified Data.Arithmetic.Types as Arithmetic
import qualified Data.Array.Indexed as V
import qualified Data.Index as Index
import qualified Data.Primitive as PM
import qualified Data.ByteArray.Builder.Small.Unsafe as BB
import qualified Data.ByteArray.Builder.Small as BBS
import qualified Data.Text.Short as TS
import qualified GHC.TypeNats as GHC

encode ::
     Int -- ^ Size hint
  -> Base.Records m
  -> PM.Array ByteArray
encode hint (Base.Records n r) = V.forget (encodeInternal hint n r)

encodeInternal ::
     Int -- ^ Size hint
  -> Arithmetic.Nat n
  -> A.Record Vectorized n m
  -> Vector n ByteArray
encodeInternal hint n (A.Record fields _ _) = runST $ do
  v <- V.new n
  offs <- V.replicateM n (0 :: Int)
  Index.ascendM
    ( \(Index.Index lt ix) -> do
      x <- PM.newByteArray hint
      V.write lt v ix x
    ) n
  goFields n v offs fields
  -- goPrefixes n v offs prefixes
  r <- V.new n
  Index.ascendM
    ( \(Index.Index lt ix) -> do
      sz <- V.read lt offs ix
      V.write lt r ix
        =<< PM.unsafeFreezeByteArray
        =<< flip PM.resizeMutableByteArray sz
        =<< V.read lt v ix
    ) n
  V.unsafeFreeze r

goFields ::
     Arithmetic.Nat n
  -> MutableVector s n (MutableByteArray s)
  -> MutableVector s n Int -- indexes into byte arrays (unchecked)
  -> A.Tree @B.FieldHeight @'A.Zero @() (ApConst2 (Vectorized n)) flds 'VecNil
  -> ST s ()
goFields n bufs offs t = do
  pushChar '{' n bufs offs
  _ <- A.iomnitraverse_ quotedEscapedFields
    (\i (Enc keyColon (Encode paste)) (ApConst2 (Vectorized v)) -> do
      when (i /= 0) $ do
        pushChar ',' n bufs offs
      pushBytes keyColon n bufs offs
      paste bufs offs n v
    ) 0 t
  pushChar '}' n bufs offs

-- goPrefixes ::
--      Arithmetic.Nat n
--   -> MutableVector s n (MutableByteArray s)
--   -> MutableVector s n Int -- indexes into byte arrays (unchecked)
--   -> A.Tree @B.PrefixHeight @'A.Zero
--        @(A.Meta B.FieldHeight B.PrefixHeight B.ManyHeight)
--        (A.ApConst1 (A.Record Vectorized n)) prefixes 'VecNil
--   -> ST s ()
-- goPrefixes n bufs offs t = A.foldMap
--   (\fng (A.ApConst1 _) -> do
--     let !key = encodePrefix 
--     Index.ascendM
--       ( \(Index.Index lt ix) -> do
--         x <- PM.newByteArray hint
--         V.write lt v ix x
--       ) n
--   ) A.FingerNil t

pushChar :: 
     Char
  -> Arithmetic.Nat n
  -> MutableVector s n (MutableByteArray s)
  -> MutableVector s n Int -- indexes into byte arrays (unchecked)
  -> ST s ()
pushChar !c !n !bufs !offs = do
  let !w = c2w c
  Index.ascendM
    ( \(Index.Index lt ix) -> do
      buf0 <- V.read lt bufs ix
      off0 <- V.read lt offs ix
      MutableByteArrayOffset buf1 off1 <-
        BB.pasteGrowST (BB.word8 w) (MutableByteArrayOffset buf0 off0)
      V.write lt bufs ix buf1
      V.write lt offs ix off1
    ) n

-- This implementation can easily be improved. The pasteGrowST
-- currently used is recursive. It would be better to perform
-- at most one growth.
pushBytes :: 
     ByteArray
  -> Arithmetic.Nat n
  -> MutableVector s n (MutableByteArray s)
  -> MutableVector s n Int -- indexes into byte arrays (unchecked)
  -> ST s ()
pushBytes !ba !n !bufs !offs = do
  let !len = PM.sizeofByteArray ba
  Index.ascendM
    ( \(Index.Index lt ix) -> do
      buf0 <- V.read lt bufs ix
      off0 <- V.read lt offs ix
      MutableByteArrayOffset buf1 off1 <- BBS.pasteGrowST 16
        (BBS.bytes (Bytes ba 0 len))
        (MutableByteArrayOffset buf0 off0)
      V.write lt bufs ix buf1
      V.write lt offs ix off1
    ) n

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Field names that have been escaped and quoted and suffixed
-- with a colon.
quotedEscapedFields :: A.Omnitree @B.FieldHeight Enc 'VecNil
quotedEscapedFields = A.omnibuild B.singFieldHeight
  (\fng ->
    let !(SBS x) = TS.toShortByteString ("\"" <> J.encodeField fng <> "\":")
     in Enc (PM.ByteArray x) (J.pasteMany fng)
  )

data Enc :: Vec B.FieldHeight Bool -> Type where
  Enc :: !ByteArray -> Encode (A.VectorizeWorld (B.Represent (B.Interpret v))) -> Enc v
