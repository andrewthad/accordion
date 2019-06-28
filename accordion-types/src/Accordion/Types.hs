{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Accordion.Types
  ( -- data types
    Nat(..)
  , SingNat(..)
  , Fin(..)
  , Gte(..)
  , SingGte(..)
  , Vec(..)
  , Set(..)
  , SetFin(..)
  , Tree(..)
  , Omnitree(..)
  , Finger(..)
  , SingBool(..)
    -- data types for instances
    -- type families
  , Empty
  , Union
  , Singleton
    -- functions
  , map
  , traverse
  , leftUnion
  , zip
  , zipM_
  , singleton
  , empty
  , omnibuild
  , omnifoldr
  ) where

import Prelude hiding (map,zip,traverse)

import Control.Applicative (liftA2)
import Data.Kind (Type)
import Data.Void (Void,absurd)

-- newtype Shown :: Type -> Type where
--   Shown :: (f (g (h a)) -> String) -> Shown f g h a

-- Natural Numbers
data Nat = Zero | Succ Nat
data SingNat :: Nat -> Type where
  SingZero :: SingNat 'Zero
  SingSucc :: SingNat n -> SingNat ('Succ n)
-- An uncommon definition of Finite Numbers. Typically, Fin 0 has
-- no inhabitants, but for our purposes, we want it to have one
-- inhabitant.
data Fin :: Nat -> Type where
  FinZero :: Fin n
  FinSucc :: Fin n -> Fin ('Succ n)
data Vec :: Nat -> Type -> Type where
  VecNil :: Vec 'Zero a
  VecCons :: a -> Vec n a -> Vec ('Succ n) a
data Set :: Nat -> Type where
  SetLeaf :: Bool -> Set 'Zero
  SetBranch :: Set n -> Set n -> Set ('Succ n)

-- A finite set. The first number is the total height (total possible
-- size is 2^h). The second number is how far down from the root
-- the node is. Common cases:
--
-- * n = 0: This node is the root node of a set.
-- * n = h: This node is a leaf node.
data SetFin (h :: Nat) (n :: Nat) where
  SetFinLeaf :: SetFin h h
  SetFinBranch :: SetFin h ('Succ n) -> SetFin h ('Succ n) -> SetFin h n
  SetFinLeft :: SetFin h ('Succ n) -> SetFin h n
  SetFinRight :: SetFin h ('Succ n) -> SetFin h n
  SetFinEmpty :: SetFin h 'Zero

-- Consider inlining SingBool into the FingerCons data constructor.
-- This could improve performance but needs to be benchmarked.
data Finger (n :: Nat) (v :: Vec n Bool) where
  FingerNil :: Finger 'Zero 'VecNil
  FingerCons :: SingBool b -> Finger n v -> Finger ('Succ n) ('VecCons b v)
data SingBool :: Bool -> Type where
  SingFalse :: SingBool 'False
  SingTrue :: SingBool 'True

type family Plus (m :: Nat) (n :: Nat) where
  Plus 'Zero n = n
  Plus ('Succ m) n = 'Succ (Plus m n)

-- Evidence that the first natural number is greater than or equal to the
-- second natural second.
data Gte :: Nat -> Nat -> Type where
  GteEq :: Gte n n
  GteGt :: Gte m ('Succ n) -> Gte m n
data SingGte (m :: Nat) (n :: Nat) (gt :: Gte m n) where
  SingGteEq :: SingGte m m 'GteEq
  SingGteGt :: forall (m :: Nat) (n :: Nat) (gt :: Gte m ('Succ n)). SingGte m ('Succ n) gt -> SingGte m n ('GteGt gt)

-- A balanced tree that always has a base-two number of leaves. The
-- type of the element at each position is determined by the interpretation
-- function applied to the finger into the tree (Vec h Bool).
data Tree (h :: Nat) (n :: Nat) (i :: Vec h Bool -> Type) (s :: SetFin h n) (v :: Vec n Bool) where
  TreeLeaf ::
       i v
    -> Tree h h i 'SetFinLeaf v
  TreeLeft ::
       Tree h ('Succ n) i sl ('VecCons 'False v)
    -> Tree h n i ('SetFinLeft sl) v
  TreeRight ::
       Tree h ('Succ n) i s ('VecCons 'True v)
    -> Tree h n i ('SetFinRight s) v
  TreeBranch ::
       Tree h ('Succ n) i sl ('VecCons 'False v)
    -> Tree h ('Succ n) i sr ('VecCons 'True v)
    -> Tree h n i ('SetFinBranch sl sr) v
  TreeEmpty :: Tree h 'Zero i 'SetFinEmpty 'VecNil

-- This is like Tree except that every element is present. It is not
-- parameterized by a Set since everything is present, but each leaf
-- does keep a type-level finger to itself.
data Omnitree (h :: Nat) (n :: Nat) (i :: Vec h Bool -> Type) (v :: Vec n Bool) where
  OmnitreeLeaf ::
       i v
    -> Omnitree h h i v
  OmnitreeBranch ::
       Omnitree h ('Succ n) i ('VecCons 'False v)
    -> Omnitree h ('Succ n) i ('VecCons 'True v)
    -> Omnitree h n i v

-- The implementation is efficient. This function typically gets
-- used to build CAFs that last for a program's lifetime, so the
-- efficiency doesn't really matter.
omnibuild :: forall h i.
     SingNat h
  -> (forall v. Finger h v -> i v)
  -> Omnitree h 'Zero i 'VecNil
omnibuild h0 f = go (gteZero h0) FingerNil where
  go :: forall m w. Gte h m -> Finger m w -> Omnitree h m i w
  go GteEq v = OmnitreeLeaf (f v)
  go (GteGt gt) v = OmnitreeBranch (go gt (FingerCons SingFalse v)) (go gt (FingerCons SingTrue v))

gteZero :: forall n. SingNat n -> Gte n 'Zero
gteZero = go GteEq where
  go :: forall m. Gte n m -> SingNat m -> Gte n 'Zero
  go gt SingZero = gt
  go gt (SingSucc x) = go (GteGt gt) x

-- singletonBase :: forall (h :: Nat) (v :: Vec h Bool) (s :: SetFin h h) (i :: Vec h Bool -> Type).
--   Finger h v -> i v -> Tree h 'Zero i (Singleton h h v ('SetFinLeaf 'True)) 'VecNil
-- singletonBase FingerNil e = TreeLeaf (Present e)
-- singletonBase (FingerCons b bs) e = _ (singletonBase bs e)

-- The way that emptyLeft and emptyRight construct the empty tree
-- is not amenable to memoization. There ought to be a better way
-- to do this that lets us cache an empty tree of each size without
-- resorting to unsafeCoerce.


empty :: forall (h :: Nat) (i :: Vec h Bool -> Type).
  Tree h 'Zero i 'SetFinEmpty 'VecNil
empty = TreeEmpty

-- emptyLeft :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: SetFin h ('Succ n)) (i :: Vec h Bool -> Type).
--      Tree h ('Succ n) i s ('VecCons 'False w)
--   -> Tree h ('Succ n) i (Empty h ('Succ n) s) ('VecCons 'True w)
-- emptyLeft (TreeLeaf _) = TreeLeaf Absent
-- emptyLeft (TreeBranch bl br) = TreeBranch (_ br) (_ br)
-- 
-- emptyTrue :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: SetFin h ('Succ n)) (i :: Vec h Bool -> Type).
--      Tree h ('Succ n) i s ('VecCons 'True w)
--   -> Tree h ('Succ n) i (Empty h ('Succ n) s) ('VecCons 'True w)
-- emptyTrue (TreeLeaf _) = TreeLeaf Absent


-- This is part of the same sharing problem that empty is involved in.
-- rightward ::
--      Tree h ('Succ n) i s ('VecCons 'False v)
--   -> Tree h ('Succ n) i s ('VecCons 'True v)
-- rightward (TreeBranch bl br) = TreeBranch bl br

-- Create a singleton tree. While the implementation of this is simple, the
-- ideas are a little confusing. The most important thing to keep in mind is
-- that the structure of this function must mirror the structure of the
-- Singleton type family. This folds over the finger, building the new tree
-- as the accumulator. We start by building a solitary leaf node and then
-- graft in empty trees that flank it as we recurse.
--
-- This function is rather general. This is needed so that it can call
-- itself recursively. End users will probably always set n=h. The two
-- finger arguments would consequently be identical.
--
-- It is not possible to generalize this function further without rewriting
-- the Singleton type family. The Zero in the resulting Tree is dictated by
-- the result kind of the Singleton type family. 
singleton :: forall (h :: Nat) (n :: Nat) (v :: Vec h Bool) (w :: Vec n Bool) (s :: SetFin h n) (gt :: Gte h n) (i :: Vec h Bool -> Type).
     SingGte h n gt -- TODO: Remove the SingGte argument. Replace with Proxy.
  -> Finger n w -- Finger to node under consideration
  -> Finger h v -- Finger to singleton leaf node, does not change while recursing
  -> Tree h n i s w
  -> Tree h 'Zero i (Singleton h n w s gt) 'VecNil
singleton _ FingerNil _ s = s
singleton sgt (FingerCons b bs) v s = case b of
  SingFalse -> singleton (SingGteGt sgt) bs v (TreeLeft s)
  SingTrue -> singleton (SingGteGt sgt) bs v (TreeRight s)

map ::
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: SetFin h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type).
     (forall (v :: Vec h Bool). Finger h v -> i v -> j v)
  -> Finger n w -- finger to the current nodes
  -> Tree h n i s w -- argument tree
  -> Tree h n j s w
map f !v (TreeLeaf x) = TreeLeaf (f v x)
map _ !_ TreeEmpty = TreeEmpty
map f !v (TreeLeft a) = TreeLeft (map f (FingerCons SingFalse v) a)
map f !v (TreeRight b) = TreeRight (map f (FingerCons SingTrue v) b)
map f !v (TreeBranch a b) = TreeBranch (map f (FingerCons SingFalse v) a) (map f (FingerCons SingTrue v) b)

traverse ::
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: SetFin h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type) (m :: Type -> Type).
     Applicative m
  => (forall (v :: Vec h Bool). Finger h v -> i v -> m (j v))
  -> Finger n w -- finger to the current nodes
  -> Tree h n i s w -- argument tree
  -> m (Tree h n j s w)
traverse _ !_ TreeEmpty = pure TreeEmpty
traverse f !v (TreeLeaf x) = fmap TreeLeaf (f v x)
traverse f !v (TreeLeft a) = fmap TreeLeft (traverse f (FingerCons SingFalse v) a)
traverse f !v (TreeRight b) = fmap TreeRight (traverse f (FingerCons SingTrue v) b)
traverse f !v (TreeBranch a b) = liftA2 TreeBranch
  (traverse f (FingerCons SingFalse v) a)
  (traverse f (FingerCons SingTrue v) b)

-- Requires the two trees to match exactly. It is possible to provide
-- a variant that does not require an exact match. Such a function would
-- use Union in the result type.
zip :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (r :: SetFin h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type)
     (k :: Vec h Bool -> Type).
     (forall (v :: Vec h Bool). Finger h v -> i v -> j v -> k v)
  -> Finger n w -- finger to the current nodes
  -> Tree h n i r w -- left tree
  -> Tree h n j r w -- right tree
  -> Tree h n k r w -- result tree
zip _ !_ TreeEmpty TreeEmpty = TreeEmpty
zip f !v (TreeLeaf x) (TreeLeaf y) = TreeLeaf (f v x y)
zip f !v (TreeLeft x) (TreeLeft y) = TreeLeft $! zip f (FingerCons SingFalse v) x y
zip f !v (TreeRight x) (TreeRight y) = TreeRight $! zip f (FingerCons SingTrue v) x y
zip f !v (TreeBranch xl xr) (TreeBranch yl yr) =
  let !l = zip f (FingerCons SingFalse v) xl yl
      !r = zip f (FingerCons SingTrue v) xr yr
   in TreeBranch l r

-- Zip the trees together. Only apply the function where
-- leaves are present in both trees.
zipM_ ::
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (r :: SetFin h n) (s :: SetFin h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type) (m :: Type -> Type).
     Monad m
  => (forall (v :: Vec h Bool). Finger h v -> i v -> j v -> m ())
  -> Finger n w -- finger to the current nodes
  -> Tree h n i r w -- left tree
  -> Tree h n j s w -- right tree
  -> m ()
zipM_ _ !_ TreeEmpty _ = pure ()
zipM_ f !v (TreeLeaf x) (TreeLeaf y) = f v x y
zipM_ _ !_ (TreeLeaf _) (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
zipM_ _ !_ (TreeLeaf _) (TreeLeft t) = absurd (impossibleGte (treeToGte t))
zipM_ _ !_ (TreeLeaf _) (TreeRight t) = absurd (impossibleGte (treeToGte t))
zipM_ _ !_ (TreeLeaf _) TreeEmpty = pure ()
zipM_ _ !_ (TreeLeft _) (TreeRight _) = pure ()
zipM_ f !v (TreeLeft a) (TreeLeft b) = zipM_ f (FingerCons SingFalse v) a b
zipM_ f !v (TreeLeft a) (TreeBranch b _) = zipM_ f (FingerCons SingFalse v) a b
zipM_ _ !_ (TreeLeft t) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
zipM_ _ !_ (TreeLeft _) TreeEmpty = pure ()
zipM_ _ !_ (TreeRight _) (TreeLeft _) = pure ()
zipM_ f !v (TreeRight a) (TreeRight b) = zipM_ f (FingerCons SingTrue v) a b
zipM_ f !v (TreeRight a) (TreeBranch _ b) = zipM_ f (FingerCons SingTrue v) a b
zipM_ _ !_ (TreeRight t) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
zipM_ _ !_ (TreeRight _) TreeEmpty = pure ()
zipM_ f !v (TreeBranch a _) (TreeLeft b) = zipM_ f (FingerCons SingFalse v) a b
zipM_ f !v (TreeBranch _ a) (TreeRight b) = zipM_ f (FingerCons SingTrue v) a b
zipM_ f !v (TreeBranch a1 a2) (TreeBranch b1 b2) = zipM_ f (FingerCons SingFalse v) a1 b1 *> zipM_ f (FingerCons SingTrue v) a2 b2
zipM_ _ !_ (TreeBranch t _) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
zipM_ _ !_ (TreeBranch _ _) TreeEmpty = pure ()

-- Left-biased union.
leftUnion :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (r :: SetFin h n) (s :: SetFin h n) (i :: Vec h Bool -> Type).
     Tree h n i r w
  -> Tree h n i s w
  -> Tree h n i (Union h n r s) w
leftUnion TreeEmpty t = t
leftUnion (TreeLeaf x) TreeEmpty = TreeLeaf x
leftUnion (TreeLeaf x) (TreeLeaf _) = TreeLeaf x
leftUnion (TreeLeft x) (TreeLeft y) = TreeLeft (leftUnion x y)
leftUnion (TreeBranch xl xr) (TreeRight yr) = TreeBranch xl (leftUnion xr yr)
leftUnion (TreeBranch xl xr) (TreeLeft yl) = TreeBranch (leftUnion xl yl) xr
leftUnion (TreeBranch xl xr) (TreeBranch yl yr) = TreeBranch (leftUnion xl yl) (leftUnion xr yr)
leftUnion (TreeLeft xl) (TreeBranch yl yr) = TreeBranch (leftUnion xl yl) yr
leftUnion (TreeRight xr) (TreeBranch yl yr) = TreeBranch yl (leftUnion xr yr)
leftUnion (TreeRight x) (TreeRight y) = TreeRight (leftUnion x y)
leftUnion (TreeLeaf _) (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
leftUnion (TreeBranch t _) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
leftUnion (TreeBranch xl xr) TreeEmpty = TreeBranch xl xr
leftUnion (TreeLeft x) TreeEmpty = TreeLeft x
leftUnion (TreeRight x) TreeEmpty = TreeRight x
leftUnion (TreeLeft x) (TreeRight y) = TreeBranch x y
leftUnion (TreeRight x) (TreeLeft y) = TreeBranch y x
leftUnion (TreeLeft t) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
leftUnion (TreeRight t) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
leftUnion (TreeLeaf _) (TreeLeft t) = absurd (impossibleGte (treeToGte t))
leftUnion (TreeLeaf _) (TreeRight t) = absurd (impossibleGte (treeToGte t))

-- Right fold over the values in a tree using an omnitree for
-- per-slot behavior.
omnifoldr ::
     Omnitree h n i v
  -> (forall (w :: Vec h Bool). i w -> j w -> b -> b)
  -> b
  -> Tree h n j s v
  -> b
omnifoldr (OmnitreeLeaf i) g b (TreeLeaf j) = g i j b
omnifoldr (OmnitreeLeaf _) _ _ (TreeLeft t) = absurd (impossibleGte (treeToGte t))
omnifoldr (OmnitreeLeaf _) _ _ (TreeRight t) = absurd (impossibleGte (treeToGte t))
omnifoldr (OmnitreeLeaf _) _ _ (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
omnifoldr (OmnitreeLeaf _) _ b TreeEmpty = b
omnifoldr (OmnitreeBranch x y) g b (TreeBranch p q) = omnifoldr x g (omnifoldr y g b q) p
omnifoldr (OmnitreeBranch x _) g b (TreeLeft p) = omnifoldr x g b p
omnifoldr (OmnitreeBranch _ y) g b (TreeRight q) = omnifoldr y g b q
omnifoldr (OmnitreeBranch x _) _ _ (TreeLeaf _) = absurd (impossibleGte (omnitreeToGte x))
omnifoldr (OmnitreeBranch _ _) _ b TreeEmpty = b

impossibleGte :: forall (m :: Nat). Gte m ('Succ m) -> Void
impossibleGte (GteGt g) = impossibleGte (predGte g)

predGte :: Gte m ('Succ n) -> Gte m n
predGte GteEq = GteGt GteEq
predGte (GteGt g) = GteGt (predGte g)

treeToGte :: Tree h ('Succ n) i s v -> Gte h ('Succ n)
treeToGte (TreeLeaf _) = GteEq
treeToGte (TreeBranch t _) = GteGt (treeToGte t)
treeToGte (TreeLeft t) = GteGt (treeToGte t)
treeToGte (TreeRight t) = GteGt (treeToGte t)

omnitreeToGte :: Omnitree h n i v -> Gte h n
omnitreeToGte (OmnitreeBranch t _) = GteGt (omnitreeToGte t)
omnitreeToGte (OmnitreeLeaf _) = GteEq

type family Empty (h :: Nat) (n :: Nat) (gt :: Gte h n) (v :: Vec n Bool) :: SetFin h n where
  Empty h h 'GteEq bs = 'SetFinLeaf
  
-- Construct a singleton set. One leaf is true and all others
-- are false.
type family Singleton (h :: Nat) (n :: Nat) (v :: Vec n Bool) (x :: SetFin h n) (gt :: Gte h n) :: SetFin h 'Zero where
  Singleton h 'Zero 'VecNil s gt = s
  Singleton h ('Succ n) ('VecCons 'False v) s gt =
    Singleton h n v ('SetFinLeft s) ('GteGt gt)
  Singleton h ('Succ n) ('VecCons 'True v) s gt =
    Singleton h n v ('SetFinRight s) ('GteGt gt)

type family Union (h :: Nat) (n :: Nat) (x :: SetFin h n) (y :: SetFin h n) :: SetFin h n where
  Union h h 'SetFinLeaf 'SetFinLeaf = 'SetFinLeaf
  Union 'Zero 'Zero 'SetFinLeaf 'SetFinEmpty = 'SetFinLeaf
  Union h 'Zero 'SetFinEmpty s = s
  Union h 'Zero ('SetFinLeft x) 'SetFinEmpty = 'SetFinLeft x
  Union h n ('SetFinBranch xl xr) ('SetFinLeft yl) = 'SetFinBranch (Union h ('Succ n) xl yl) xr
  Union h n ('SetFinBranch xl xr) ('SetFinRight yr) = 'SetFinBranch xl (Union h ('Succ n) xr yr)
  Union h n ('SetFinLeft xl) ('SetFinBranch yl yr) = 'SetFinBranch (Union h ('Succ n) xl yl) yr
  Union h n ('SetFinRight xr) ('SetFinBranch yl yr) = 'SetFinBranch yl (Union h ('Succ n) xr yr)
  Union h n ('SetFinLeft x) ('SetFinLeft y) = 'SetFinLeft (Union h ('Succ n) x y)
  Union h n ('SetFinLeft x) ('SetFinRight y) = 'SetFinBranch x y
  Union h n ('SetFinRight x) ('SetFinRight y) = 'SetFinRight (Union h ('Succ n) x y)
  Union h n ('SetFinRight x) ('SetFinLeft y) = 'SetFinBranch y x
  Union h 'Zero ('SetFinRight x) 'SetFinEmpty = 'SetFinRight x
  Union h 'Zero ('SetFinBranch xl xr) 'SetFinEmpty = 'SetFinBranch xl xr
  Union h n ('SetFinBranch xl xr) ('SetFinBranch yl yr) = 'SetFinBranch (Union h ('Succ n) xl yl) (Union h ('Succ n) xr yr)
