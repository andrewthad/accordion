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
  , single
  , nestedSingle
  , equals
  , equalsHetero
  , substitute
  ) where

import Data.Kind (Type)
import Data.Monoid (All(All),getAll)
import Accordion.Types (ApConst1(..),ApConst2(..))
import Control.Monad.ST (runST)
import GHC.TypeLits (type (+))
import Accordion.Internal (Vectorized(..))
import Data.Functor.Const (Const(Const),getConst)
import Data.Arithmetic.Types ((:=:),Nat)
import Data.Type.Equality ((:~:)(Refl))

import qualified Accordion.Types as A
import qualified Accordion.World as A
import qualified Data.Primitive as PM
import qualified Accordion.Base.Signature as S
import qualified Data.Array.Indexed as V
import qualified World.Bool as Bool
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

substitute :: n :=: m -> Optionals n rs -> Optionals m rs
substitute e (Optionals xs) = Optionals (substituteGo e xs)

-- Unfortunately, it is not possible to implement testEquality
-- unless we accept a performance hit.
--
-- testEquality ::
--      Optionals n rs
--   -> Optionals m ss
--   -> Maybe (rs :~: ss)
-- testEquality (Optionals x) (Optionals y) = testEqualityGo x y
-- 
-- testEqualityGo ::
--      A.Record Vectorized A.WithBools n rs
--   -> A.Record Vectorized A.WithBools m ss
--   -> Maybe (rs :~: ss)
-- testEqualityGo (A.Record a1 b1 _) (A.Record a2 b2 _) = do
--   Refl <- A.testEquality (\(ApConst2 x) (ApConst2 y) -> _) a1 a2
--   error "uhoentuh"

substituteGo ::
     n :=: m
  -> A.Record Vectorized A.WithBools n rs
  -> A.Record Vectorized A.WithBools m rs
substituteGo e (A.Record a1 b1 _) = A.Record
  ( A.map
    (\fng (A.ApConst2 (A.WithBools bs1 (Vectorized v1))) ->
      (A.ApConst2 (A.WithBools (Bool.substitute e bs1) (Vectorized (A.substituteVector (S.represent (S.interpret fng)) e v1))))
    ) A.FingerNil a1
  )
  ( A.map
    (\fng (A.ApConst1 r) -> A.ApConst1 (substituteGo e r)
    ) A.FingerNil b1
  )
  (error "substituteGo: handle collections")

equalsHetero ::
     Nat n
  -> Optionals n rs
  -> Optionals n ss
  -> Bool
equalsHetero n (Optionals x) (Optionals y) =
  getAll (getConst (equalsHeteroGo n x y))

-- Returns false if the types do not match.
-- TODO: handle collections correctly
equalsHeteroGo :: forall
  (n :: GHC.Nat)
  (rs :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight)
  (ss :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
     Nat n
  -> A.Record Vectorized A.WithBools n rs
  -> A.Record Vectorized A.WithBools n ss
  -> Const All ()
equalsHeteroGo n (A.Record a1 b1 _) (A.Record a2 b2 _) =
  A.zipAllM_
    (\_ _ -> Const (All False))
    (\_ _ -> Const (All False))
    (\fng (A.ApConst2 (A.WithBools bs1 (Vectorized v1))) (A.ApConst2 (A.WithBools bs2 (Vectorized v2))) ->
      Const $
        All (A.eqVectors (S.represent (S.interpret fng)) n v1 v2)
        <>
        All (Bool.equals n bs1 bs2)
    )
    A.FingerNil a1 a2
  <>
  A.zipAllM_
    (\_ _ -> Const (All False))
    (\_ _ -> Const (All False))
    (\_ (ApConst1 x) (ApConst1 y) -> equalsHeteroGo n x y)
    A.FingerNil b1 b2

-- TODO: handle vector fields correctly
equals ::
     Nat n
  -> Optionals n rs
  -> Optionals n rs
  -> Bool
equals n (Optionals x) (Optionals y) = getAll (getConst (equalsGoA n x y))

equalsGoA :: 
     Nat n
  -> A.Record Vectorized A.WithBools n m
  -> A.Record Vectorized A.WithBools n m
  -> Const All ()
equalsGoA n (A.Record a1 b1 _) (A.Record a2 b2 _) =
  A.zipSameM_
    (\fng (A.ApConst2 (A.WithBools bs1 (Vectorized v1))) (A.ApConst2 (A.WithBools bs2 (Vectorized v2))) ->
      Const $
        All (A.eqVectors (S.represent (S.interpret fng)) n v1 v2)
        <>
        All (Bool.equals n bs1 bs2)
    )
    A.FingerNil a1 a2
  <>
  A.zipSameM_ (\_ (ApConst1 x) (ApConst1 y) -> equalsGoA n x y) A.FingerNil b1 b2

-- This does not do what it really should. Overlap probably
-- should not be left-biased in the way in currently is.
union ::
     Optionals n rs
  -> Optionals n ss
  -> Optionals n (A.UnionMeta rs ss)
union (Optionals x) (Optionals y) = Optionals (A.unionRecord x y)

append ::
     Nat n
  -> Nat m
  -> Optionals n rs
  -> Optionals m rs
  -> Optionals (n + m) rs
append !n !m (Optionals x) (Optionals y) = Optionals (appendGo n m x y)

appendGo :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
     Nat na
  -> Nat nb
  -> A.Record Vectorized A.WithBools na m
  -> A.Record Vectorized A.WithBools nb m
  -> A.Record Vectorized A.WithBools (na + nb) m
appendGo !na !nb (A.Record b1 c1 d1) (A.Record b2 c2 d2) =
  A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (A.WithBools bs1 (Vectorized v1))) (ApConst2 (A.WithBools bs2 (Vectorized v2))) ->
      ApConst2 (A.WithBools
        (Bool.append na nb bs1 bs2)
        (Vectorized (A.appendVectors (S.represent (S.interpret fng)) na nb v1 v2))
      )) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendGo na nb p1 p2) A.FingerNil c1 c2
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
      ApConst2 (A.Apply2 (Vectorized (A.appendVectors (S.represent (S.interpret fng)) x1 x2 v1 v2)))) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendMandatory x1 x2 p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )

empty :: Optionals n A.MetaEmpty
empty = Optionals (A.Record A.TreeEmpty A.TreeEmpty A.TreeEmpty)

single ::
     A.Finger v
  -> S.Ground (S.Interpret v)
  -> Optionals 1 (A.MetaFields (A.Singleton v ('A.MapLeaf '())))
single fng val = column fng
  (Bool.singleton True)
  (A.singletonVector (S.represent u) (S.toInternal u val))
  where
  u = S.interpret fng

nestedSingle ::
     A.Rec (A.Finger @S.PrefixHeight) ps
  -> A.Finger v
  -> S.Ground (S.Interpret v)
  -> Optionals 1 (A.NestedSingleton ps v '())
nestedSingle A.RecNil fng val = single fng val
nestedSingle (A.RecCons r rs) fng val = Optionals $ A.Record
  A.TreeEmpty
  (A.singleton r (A.TreeLeaf (ApConst1 (getOptionals (nestedSingle rs fng val)))))
  A.TreeEmpty

column ::
     A.Finger v
  -> Bool.Vector n
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
  -> Bool.Vector n
  -> A.VectorizeWorld (S.Represent (S.Interpret v)) n
  -> Optionals n (A.NestedSingleton ps v '())
nestedColumn A.RecNil fng bs vec = column fng bs vec
nestedColumn (A.RecCons r rs) fng bs vec = Optionals $ A.Record
  A.TreeEmpty
  (A.singleton r (A.TreeLeaf (ApConst1 (getOptionals (nestedColumn rs fng bs vec)))))
  A.TreeEmpty

appendMandatory :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
     Nat na
  -> Nat nb
  -> A.Record Vectorized A.Apply2 na m
  -> A.Record Vectorized A.Apply2 nb m
  -> A.Record Vectorized A.Apply2 (na + nb) m
appendMandatory !na !nb (A.Record b1 c1 d1) (A.Record b2 c2 d2) =
  A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (A.Apply2 (Vectorized v1))) (ApConst2 (A.Apply2 (Vectorized v2))) ->
      ApConst2 (A.Apply2 (Vectorized (A.appendVectors (S.represent (S.interpret fng)) na nb v1 v2)))) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendMandatory na nb p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )

