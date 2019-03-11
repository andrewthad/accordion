{-# language DataKinds #-}
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
  , Vec(..)
  , Set(..)
  , SetFin(..)
  , Optional(..)
  , Tree(..)
    -- type families
  , Empty
  , Singleton
  , SetFinToGte
  ) where

import Data.Kind (Type)
import Data.Proxy

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
  SetLeaf :: Bool -> Set Zero
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
data Optional :: Bool -> Type -> Type where
  Absent :: Optional 'False a
  Present :: a -> Optional 'True a
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

-- A balanced tree that always has a base-two number of elements. The
-- type of the element at each position is determined by the interpretation
-- function applied to the finger into the tree (Vec h Bool).
data Tree (h :: Nat) (n :: Nat) (i :: Vec h Bool -> Type) (s :: SetFin h n) (v :: Vec n Bool) where
  TreeLeaf ::
       Optional b (i v)
    -> Tree h h i ('SetFinLeaf b) v
  TreeBranch ::
       Tree h ('Succ n) i sl ('VecCons False v)
    -> Tree h ('Succ n) i sr ('VecCons True v)
    -> Tree h n i ('SetFinBranch sl sr) v

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
-- 
singleton :: forall (h :: Nat) (n :: Nat) (v :: Vec h Bool) (w :: Vec n Bool) (s :: SetFin h n) (gt :: Gte h n) (i :: Vec h Bool -> Type).
     SingGte h n gt
  -> Finger n w -- Finger to node under consideration
  -> Finger h v -- Finger to singleton leaf node, does not change while recursing
  -> i v -- Value in the only enabled tree leaf node
  -> Tree h n i s w
  -> Tree h 'Zero i (Singleton h n w s gt) 'VecNil
singleton sgt FingerNil v e s = s
singleton sgt (FingerCons b bs) v e s = case b of
  SingFalse -> singleton (SingGteGt sgt) bs v e (TreeBranch s (empty sgt))
  SingTrue -> singleton (SingGteGt sgt) bs v e (TreeBranch (empty sgt) s)

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
    -- Singleton h n v ('SetFinBranch s (Empty h ('Succ n) (SetFinToGte h ('Succ n) s)))
    -- Singleton h n v ('SetFinBranch s (Empty h ('Succ n) s))
    Singleton h n v ('SetFinBranch s (Empty h ('Succ n) gt ('VecCons 'True v))) ('GteGt gt)
  Singleton h ('Succ n) ('VecCons 'True v) s gt =
    -- Singleton h n v ('SetFinBranch (Empty h ('Succ n) (SetFinToGte h ('Succ n) s)) s)
    -- Singleton h n v ('SetFinBranch (Empty h ('Succ n) s) s)
    Singleton h n v ('SetFinBranch (Empty h ('Succ n) gt ('VecCons 'False v)) s) ('GteGt gt)

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

