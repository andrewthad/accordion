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

module Accordion.Optionals
  ( Optionals(..)
    -- Functions
  , empty
  , union
  , append
  , column
  , nestedColumn
  ) where

import Data.Kind (Type)
import Accordion.Types (ApConst1(..),ApConst2(..))
import Control.Monad.ST (runST)
import GHC.TypeLits (type (+))
import Accordion.Internal (Vectorized(..))

import qualified Accordion.Types as A
import qualified Accordion.World as A
import qualified Data.Primitive as PM
import qualified Accordion.Base.Signature as S
import qualified Data.Array.Indexed as V
import qualified Data.Array.Bool as Bool
import qualified GHC.TypeNats as GHC
import qualified Data.Arithmetic.Types as Arithmetic
import qualified Data.Arithmetic.Nat as Nat
import qualified Data.Index as Index

newtype Optionals ::
     GHC.Nat
  -> A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight
  -> Type
  where
  Optionals ::
       {getOptionals :: A.Record Vectorized A.WithBools n m} -> Optionals n m

-- This does not do what it really should. Overlap probably
-- should not be left-biased in the way in currently is.
union ::
     Optionals n rs
  -> Optionals n ss
  -> Optionals n (A.UnionMeta rs ss)
union (Optionals x) (Optionals y) = Optionals (A.unionRecord x y)

append ::
     Optionals n rs
  -> Optionals m rs
  -> Optionals (n + m) rs
append (Optionals x) (Optionals y) = Optionals (appendGo x y)

appendGo :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
     A.Record Vectorized A.WithBools na m
  -> A.Record Vectorized A.WithBools nb m
  -> A.Record Vectorized A.WithBools (na + nb) m
appendGo (A.Record b1 c1 d1) (A.Record b2 c2 d2) =
  A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (A.WithBools bs1 (Vectorized v1))) (ApConst2 (A.WithBools bs2 (Vectorized v2))) ->
      ApConst2 (A.WithBools
        (Bool.append bs1 bs2)
        (Vectorized (A.appendVectors (S.represent (S.interpret fng)) v1 v2))
      )) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendGo p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )

appendCollections :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
     A.Collection Vectorized A.Apply2 na m
  -> A.Collection Vectorized A.Apply2 nb m
  -> A.Collection Vectorized A.Apply2 (na + nb) m
appendCollections
  (A.Collection x1 a1 (A.Record b1 c1 d1))
  (A.Collection x2 a2 (A.Record b2 c2 d2)) =
  A.Collection (Nat.plus x1 x2)
    ( V.append
      (fmap (fmap (Index.incrementLimitR x2)) a1)
      (fmap (fmap (\i -> Index.incrementL x1 i)) a2)
    ) $ A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (A.Apply2 (Vectorized v1))) (ApConst2 (A.Apply2 (Vectorized v2))) ->
      ApConst2 (A.Apply2 (Vectorized (A.appendVectors (S.represent (S.interpret fng)) v1 v2)))) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendMandatory p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )

empty :: Optionals n A.MetaEmpty
empty = Optionals (A.Record A.TreeEmpty A.TreeEmpty A.TreeEmpty)

column ::
     A.Finger v
  -> Bool.BoolVector n
  -> A.VectorizeWorld (S.Represent (S.Interpret v)) n
  -> Optionals n (A.MetaFields (A.Singleton v ('A.MapLeaf '())))
column fng bs vec = Optionals $ A.Record
  ( A.singleton @S.FieldHeight @_ @() @_ @('A.MapLeaf '()) fng
    (A.TreeLeaf (ApConst2 (A.WithBools bs (Vectorized vec))))
  )
  A.TreeEmpty
  A.TreeEmpty

nestedColumn ::
     A.Rec (A.Finger @S.PrefixHeight) ps
  -> A.Finger v
  -> Bool.BoolVector n
  -> A.VectorizeWorld (S.Represent (S.Interpret v)) n
  -> Optionals n (A.NestedSingleton ps v '())
nestedColumn A.RecNil fng bs vec = column fng bs vec
nestedColumn (A.RecCons r rs) fng bs vec = Optionals $ A.Record
  A.TreeEmpty
  (A.singleton r (A.TreeLeaf (ApConst1 (getOptionals (nestedColumn rs fng bs vec)))))
  A.TreeEmpty

appendMandatory :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
  A.Record Vectorized A.Apply2 na m -> A.Record Vectorized A.Apply2 nb m -> A.Record Vectorized A.Apply2 (na + nb) m
appendMandatory (A.Record b1 c1 d1) (A.Record b2 c2 d2) =
  A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (A.Apply2 (Vectorized v1))) (ApConst2 (A.Apply2 (Vectorized v2))) ->
      ApConst2 (A.Apply2 (Vectorized (A.appendVectors (S.represent (S.interpret fng)) v1 v2)))) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendMandatory p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )

