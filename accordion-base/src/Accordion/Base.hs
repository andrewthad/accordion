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

module Accordion.Base
  ( Record(..)
  , Records(..)
  , Vectorized(..)
    -- Functions
  , one
  , field
  , union
  ) where

import Data.Kind (Type)
import Accordion.Types (ApConst1(..),ApConst2(..))
import Control.Monad.ST (runST)
import GHC.TypeLits (type (+))

import qualified Accordion.Types as A
import qualified Accordion.World as A
import qualified Data.Primitive as PM
import qualified Accordion.Base.Signature as S
import qualified Data.Array.Indexed as V
import qualified GHC.TypeNats as GHC
import qualified Data.Arithmetic.Types as Arithmetic
import qualified Data.Arithmetic.Nat as Nat
import qualified Data.Index as Index

newtype Record :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight -> Type where
  Record :: A.Record Vectorized 1 m -> Record m

data Records :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight -> Type where
  Records :: Arithmetic.Nat n -> A.Record Vectorized n m -> Records m

newtype Vectorized :: GHC.Nat -> A.Vec S.FieldHeight Bool -> Type where
  Vectorized :: A.VectorizeWorld (S.Represent (S.Interpret v)) n -> Vectorized n v

instance Semigroup (Records m) where
  Records n1 a1 <> Records n2 a2 = Records (Nat.plus n1 n2) (appendMyRecords a1 a2)

one :: Record m -> Records m
one (Record (A.Record x y z)) = Records Nat.one (A.Record x y z)

field ::
     A.Finger S.FieldHeight v
  -> S.Ground (S.Interpret v)
  -> Record (A.MetaFields (A.Singleton v ('A.MapLeaf '())))
field fng x = Record $ A.Record
  ( A.singleton @S.FieldHeight @_ @() @_ @('A.MapLeaf '()) fng
    (A.TreeLeaf (ApConst2 (Vectorized (A.singletonVector (S.represent u) (S.toInternal u x)))))
  )
  A.TreeEmpty
  A.TreeEmpty
  where
  u = S.interpret fng

union :: Record rs -> Record ss -> Record (A.UnionMeta rs ss)
union (Record x) (Record y) = Record (A.unionRecord x y)

appendMyRecords :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
  A.Record Vectorized na m -> A.Record Vectorized nb m -> A.Record Vectorized (na + nb) m
appendMyRecords (A.Record b1 c1 d1) (A.Record b2 c2 d2) =
  A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (Vectorized v1)) (ApConst2 (Vectorized v2)) ->
      ApConst2 (Vectorized (A.appendVectors (S.represent (S.interpret fng)) v1 v2))) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendMyRecords p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )

appendCollections :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
     A.Collection Vectorized na m
  -> A.Collection Vectorized nb m
  -> A.Collection Vectorized (na + nb) m
appendCollections
  (A.Collection x1 a1 (A.Record b1 c1 d1))
  (A.Collection x2 a2 (A.Record b2 c2 d2)) =
  let sza1 = V.length a1
      sza2 = V.length a2
  in
  A.Collection (Nat.plus x1 x2)
    ( V.append
      (fmap (fmap (Index.incrementLimitR x2)) a1)
      (fmap (fmap (\i -> Index.incrementL x1 i)) a2)
    ) $ A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (Vectorized v1)) (ApConst2 (Vectorized v2)) ->
      ApConst2 (Vectorized (A.appendVectors (S.represent (S.interpret fng)) v1 v2))) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendMyRecords p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )

singletonArray :: a -> PM.Array a
singletonArray x = runST
  (PM.newArray 1 x >>= PM.unsafeFreezeArray)

ascendingArray :: Arithmetic.Nat n -> PM.Array (Index.Index n)
ascendingArray n = V.forget $ runST $ do
  m <- V.new n
  Index.ascendM (\ix@(Index.Index lt p) -> V.write lt m p ix) n
  V.unsafeFreeze m
