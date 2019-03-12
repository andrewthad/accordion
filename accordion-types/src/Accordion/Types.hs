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
  , Fin(..)
  , Gte(..)
  , SingGte(..)
  , Vec(..)
  , Set(..)
  , SetFin(..)
  , Optional(..)
  , Tree(..)
  , Omnitree(..)
  , Finger(..)
  , SingBool(..)
    -- data types for instances
  , Shown(..)
    -- type families
  , Empty
  , Union
  , Singleton
  , SetFinToGte
    -- functions
  , leftUnion
  , singleton
  , empty
  ) where

import Data.Kind (Type)
import Data.Void (Void,absurd)

newtype Shown (k :: Type) (f :: k -> Type) (a :: k) where
  Shown :: forall (k :: Type) (f :: k -> Type) (a :: k).
       (f a -> String)
    -> Shown k f a

-- Natural Numbers
data Nat = Zero | Succ Nat
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
  SetFinLeaf :: Bool -> SetFin h h
  SetFinBranch :: SetFin h ('Succ n) -> SetFin h ('Succ n) -> SetFin h n

-- TODO: Get rid of Optional. This will have to wait until Tree is
-- refactored to prune empty branches. Then, all leaves will have
-- values.
data Optional :: Bool -> Type -> Type where
  Absent :: Optional 'False a
  Present :: a -> Optional 'True a
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
       Optional b (i v)
    -> Tree h h i ('SetFinLeaf b) v
  TreeBranch ::
       Tree h ('Succ n) i sl ('VecCons 'False v)
    -> Tree h ('Succ n) i sr ('VecCons 'True v)
    -> Tree h n i ('SetFinBranch sl sr) v

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

-- singletonBase :: forall (h :: Nat) (v :: Vec h Bool) (s :: SetFin h h) (i :: Vec h Bool -> Type).
--   Finger h v -> i v -> Tree h 'Zero i (Singleton h h v ('SetFinLeaf 'True)) 'VecNil
-- singletonBase FingerNil e = TreeLeaf (Present e)
-- singletonBase (FingerCons b bs) e = _ (singletonBase bs e)

-- The way that emptyLeft and emptyRight construct the empty tree
-- is not amenable to memoization. There ought to be a better way
-- to do this that lets us cache an empty tree of each size without
-- resorting to unsafeCoerce.
empty :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (gt :: Gte h n) (i :: Vec h Bool -> Type).
     SingGte h n gt
  -> Tree h n i (Empty h n gt w) w
empty SingGteEq = TreeLeaf Absent
empty (SingGteGt sgt) = TreeBranch (empty sgt) (empty sgt)

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
  -> i v -- Value in the only enabled tree leaf node
  -> Tree h n i s w
  -> Tree h 'Zero i (Singleton h n w s gt) 'VecNil
singleton _ FingerNil _ _ s = s
singleton sgt (FingerCons b bs) v e s = case b of
  SingFalse -> singleton (SingGteGt sgt) bs v e (TreeBranch s (empty sgt))
  SingTrue -> singleton (SingGteGt sgt) bs v e (TreeBranch (empty sgt) s)

-- Left-biased union.
leftUnion :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (r :: SetFin h n) (s :: SetFin h n) (i :: Vec h Bool -> Type).
     Tree h n i r w
  -> Tree h n i s w
  -> Tree h n i (Union h n r s) w
leftUnion (TreeLeaf x) (TreeLeaf y) = case x of
  Present v -> TreeLeaf (Present v)
  Absent -> TreeLeaf y
leftUnion (TreeBranch xl xr) (TreeBranch yl yr) = TreeBranch (leftUnion xl yl) (leftUnion xr yr)
leftUnion (TreeLeaf _) (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
leftUnion (TreeBranch t _) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))

impossibleGte :: forall (m :: Nat). Gte m ('Succ m) -> Void
impossibleGte (GteGt g) = impossibleGte (predGte g)

predGte :: Gte m ('Succ n) -> Gte m n
predGte GteEq = GteGt GteEq
predGte (GteGt g) = GteGt (predGte g)

treeToGte :: Tree h n i s v -> Gte h n
treeToGte (TreeLeaf _) = GteEq
treeToGte (TreeBranch t _) = GteGt (treeToGte t)

-- This uses a non-linear pattern match. Might be bad. Not sure.
-- I think the appropriate evidence will be brought into scope
-- when this is being used, but we'll see.
-- type family Duplicate (h :: Nat) (n :: Nat) (s :: SetFin h n) :: SetFin ('Succ h) n where
--   Duplicate h h (SetFinLeaf b) = SetFinBranch (SetFinLeaf b) (SetFinLeaf b)
--   Duplicate h n (SetFinBranch x y) = SetFinBranch (Duplicate h ('Succ n) x) (Duplicate h ('Succ n) y)

-- Construct an empty set of a given height. The third argument is only used
-- as evidence that n <= h.
-- type family Empty (h :: Nat) (n :: Nat) (s :: SetFin h n) :: SetFin h n where
--   Empty h h ('SetFinLeaf _) = 'SetFinLeaf 'False
--   Empty h n ('SetFinBranch bl br) = 'SetFinBranch (Empty h ('Succ n) bl) (Empty h ('Succ n) br)

type family Empty (h :: Nat) (n :: Nat) (gt :: Gte h n) (v :: Vec n Bool) :: SetFin h n where
  Empty h h 'GteEq bs = 'SetFinLeaf 'False
  Empty h n ('GteGt gt) bs = 'SetFinBranch
    (Empty h ('Succ n) gt ('VecCons 'False bs))
    (Empty h ('Succ n) gt ('VecCons 'True bs))
  
-- type family Empty (h :: Nat) (n :: Nat) (x :: SetFin h n) :: SetFin h n where
--   Empty h h ('SetFinLeaf _) = SetFinLeaf 'False
--   Empty ('Succ h) n ('SetFinBranch s _) = Duplicate ('Succ h) ('Succ n) (Empty h n s) -- SetFinBranch (Empty h ('Succ n)) (Empty h ('Succ n))

-- Construct an empty set of a given height. The third argument is only used
-- as evidence that n <= h.
-- type family Empty (h :: Nat) (n :: Nat) (x :: SetFin h n) :: SetFin h n where
--   Empty h h ('SetFinLeaf _) = SetFinLeaf 'False
--   Empty ('Succ h) n ('SetFinBranch s _) = Duplicate ('Succ h) ('Succ n) (Empty ('Succ h) ('Succ n) s) -- SetFinBranch (Empty h ('Succ n)) (Empty h ('Succ n))

-- TODO: Eliminate the need for this by inlining this deconstruction into
-- the type family Empty. This will make it easier to write functions
-- that work at the value level, and they will perform better since they
-- will not need to allocate evidence.
type family SetFinToGte (h :: Nat) (n :: Nat) (s :: SetFin h n) :: Gte h n where
  SetFinToGte h h ('SetFinLeaf b) = 'GteEq
  SetFinToGte h n ('SetFinBranch s _) = 'GteGt (SetFinToGte h ('Succ n) s)

-- Construct a singleton set. One leaf is true and all others
-- are false.
type family Singleton (h :: Nat) (n :: Nat) (v :: Vec n Bool) (x :: SetFin h n) (gt :: Gte h n) :: SetFin h 'Zero where
  Singleton h 'Zero 'VecNil s gt = s
  Singleton h ('Succ n) ('VecCons 'False v) s gt =
    Singleton h n v ('SetFinBranch s (Empty h ('Succ n) gt ('VecCons 'True v))) ('GteGt gt)
  Singleton h ('Succ n) ('VecCons 'True v) s gt =
    Singleton h n v ('SetFinBranch (Empty h ('Succ n) gt ('VecCons 'False v)) s) ('GteGt gt)

type family Union (h :: Nat) (n :: Nat) (x :: SetFin h n) (y :: SetFin h n) :: SetFin h n where
  Union h h ('SetFinLeaf x) ('SetFinLeaf y) = 'SetFinLeaf (Or x y)
  Union h n ('SetFinBranch xl xr) ('SetFinBranch yl yr) = 'SetFinBranch (Union h ('Succ n) xl yl) (Union h ('Succ n) xr yr)

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False x = x

-- Construct a singleton set. One leaf is true and all others
-- are false. Unfortunately, we cannot write this since
-- the constructive approach to addition makes things awful.
-- type family Singleton (h :: Nat) (p :: Nat) (v :: Vec h Bool) (x :: Set p) :: Set (Plus h p) where
--   Singleton 'Zero p 'VecNil s = s
--   Singleton ('Succ h) p ('VecCons 'False v) s =
--     Singleton h ('Succ p) v (SetBranch s (Empty p))
-- type family Empty (h :: Nat) :: Set h where
--   Empty 'Zero = SetLeaf 'False
--   Empty ('Succ h) = SetBranch (Empty h) (Empty h)

