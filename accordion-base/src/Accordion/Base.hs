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
  , Optionals(..)
  , OptionalsN(..)
  , Vectorized(..)
    -- Functions
  , empty
  , emptyOptN
  , one
  , opt
  , optColumn
  , field
  , union
    -- Conversion
  , append
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
import qualified Data.Array.Bool as Bool
import qualified GHC.TypeNats as GHC
import qualified Data.Arithmetic.Types as Arithmetic
import qualified Data.Arithmetic.Nat as Nat
import qualified Data.Index as Index

-- newtype Label :: Vec FieldHeight Bool -> Type where
--   Label :: Field FieldHeight Bool 

newtype Record :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight -> Type where
  Record :: A.Record Vectorized A.Apply2 1 m -> Record m

data Records :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight -> Type where
  Records ::
       Arithmetic.Nat n
    -> A.Record Vectorized A.Apply2 n m
    -> Records m

data Optionals :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight -> Type where
  Optionals ::
       Arithmetic.Nat n
    -> A.Record Vectorized A.WithBools n m
    -> Optionals m

newtype OptionalsN ::
     GHC.Nat
  -> A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight
  -> Type
  where
  OptionalsN :: A.Record Vectorized A.WithBools n m -> OptionalsN n m

newtype Vectorized :: GHC.Nat -> A.Vec S.FieldHeight Bool -> Type where
  Vectorized :: A.VectorizeWorld (S.Represent (S.Interpret v)) n -> Vectorized n v

instance Semigroup (Records m) where
  Records n1 a1 <> Records n2 a2 = Records (Nat.plus n1 n2) (appendMandatory a1 a2)

instance Semigroup (Optionals m) where
  Optionals n1 a1 <> Optionals n2 a2 = Optionals (Nat.plus n1 n2) (appendOptional a1 a2)

one :: Record m -> Records m
one (Record (A.Record x y z)) = Records Nat.one (A.Record x y z)

opt :: Record m -> Optionals m
opt (Record (A.Record x A.TreeEmpty A.TreeEmpty)) = Optionals Nat.one $ A.Record
  (A.map (\_ (ApConst2 (A.Apply2 x)) ->
    ApConst2 (A.WithBools (Bool.singleton True) x)) A.FingerNil x
  )
  A.TreeEmpty
  A.TreeEmpty

field ::
     A.Finger v
  -> S.Ground (S.Interpret v)
  -> Record (A.MetaFields (A.Singleton v ('A.MapLeaf '())))
field fng x = Record $ A.Record
  ( A.singleton @S.FieldHeight @_ @() @_ @('A.MapLeaf '()) fng
    (A.TreeLeaf (ApConst2 (A.Apply2 (Vectorized (A.singletonVector (S.represent u) (S.toInternal u x))))))
  )
  A.TreeEmpty
  A.TreeEmpty
  where
  u = S.interpret fng

optColumn ::
     A.Finger v
  -> Bool.BoolVector n
  -> A.VectorizeWorld (S.Represent (S.Interpret v)) n
  -> OptionalsN n (A.MetaFields (A.Singleton v ('A.MapLeaf '())))
optColumn fng bs vec = OptionalsN $ A.Record
  ( A.singleton @S.FieldHeight @_ @() @_ @('A.MapLeaf '()) fng
    (A.TreeLeaf (ApConst2 (A.WithBools bs (Vectorized vec))))
  )
  A.TreeEmpty
  A.TreeEmpty
  where
  u = S.interpret fng



union :: Record rs -> Record ss -> Record (A.UnionMeta rs ss)
union (Record x) (Record y) = Record (A.unionRecord x y)

empty :: Record A.MetaEmpty
empty = Record (A.Record A.TreeEmpty A.TreeEmpty A.TreeEmpty)

emptyOptN :: OptionalsN n A.MetaEmpty
emptyOptN = OptionalsN (A.Record A.TreeEmpty A.TreeEmpty A.TreeEmpty)

-- This is currently broken when using subfields.
append :: Optionals rs -> Optionals ss -> Optionals (A.UnionMeta rs ss)
append (Optionals n1 (A.Record b1 A.TreeEmpty A.TreeEmpty)) (Optionals n2 (A.Record b2 A.TreeEmpty A.TreeEmpty)) = Optionals (Nat.plus n1 n2) $
  A.Record 
    (A.union
      (\fng (ApConst2 (A.WithBools bs1 (Vectorized x1))) (ApConst2 (A.WithBools bs2 (Vectorized x2))) ->
        ApConst2 (A.WithBools (Bool.append bs1 bs2) (Vectorized (A.appendVectors (S.represent (S.interpret fng)) x1 x2)))
      )
      (\fng (ApConst2 (A.WithBools bs1 (Vectorized x1))) -> 
        ApConst2 (A.WithBools (Bool.append bs1 (Bool.replicate n2 False)) (Vectorized (A.rightPadVector (S.represent (S.interpret fng)) n2 x1)))
      )
      (\fng (ApConst2 (A.WithBools bs2 (Vectorized x2))) ->
        ApConst2 (A.WithBools (Bool.append (Bool.replicate n1 False) bs2) (Vectorized (A.leftPadVector (S.represent (S.interpret fng)) n1 x2)))
      ) A.FingerNil b1 b2
    )
    A.TreeEmpty
    A.TreeEmpty
    -- (A.union _ _ _)
    -- _

-- Discard rows in which any field is missing.
-- require :: Optionals rs -> Records rs
-- require

appendOptional :: forall na nb (m :: A.Meta S.FieldHeight S.PrefixHeight S.ManyHeight).
  A.Record Vectorized A.WithBools na m -> A.Record Vectorized A.WithBools nb m -> A.Record Vectorized A.WithBools (na + nb) m
appendOptional (A.Record b1 c1 d1) (A.Record b2 c2 d2) =
  A.Record @S.FieldHeight @S.PrefixHeight @S.ManyHeight @Vectorized
    (A.zip (\fng (ApConst2 (A.WithBools bs1 (Vectorized v1))) (ApConst2 (A.WithBools bs2 (Vectorized v2))) ->
      ApConst2 (A.WithBools
        (Bool.append bs1 bs2)
        (Vectorized (A.appendVectors (S.represent (S.interpret fng)) v1 v2))
      )) A.FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendOptional p1 p2) A.FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) A.FingerNil d1 d2
    )


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
