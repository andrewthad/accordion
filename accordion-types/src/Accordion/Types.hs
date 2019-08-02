{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Accordion.Types
  ( -- data types
    Nat(..)
  , SingNat(..)
  , Fin(..)
  , Collection(..)
  , Record(..)
  , Gte(..)
  , ApConst1(..)
  , ApConst2(..)
  , Meta(..)
  , MetaEmpty
  , MetaFields
  , Map(..)
  , SingGte(..)
  , Vec(..)
  , Set(..)
  , Subset(..)
  , Tree(..)
  , Omnitree(..)
  , Finger(..)
  , SingBool(..)
  , Elem(..)
  , FieldList
    -- data types for instances
    -- type families
  , Empty
  , Union
  , UnionMeta
  , Singleton
    -- functions
  , map
  , traverse
  , itraverse_
  , traverseF
  , foldMap
  , foldMapF
  , leftUnion
  , unionRecord
  , zip
  , zipM_
  , singleton
  , empty
  , omnibuild
  , omnifoldr
  , omnisubset
  , iomnitraverse_
    -- getter and setter
  , get
  ) where

import Prelude hiding (map,zip,traverse,foldMap)

import Control.Applicative (liftA2)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Void (Void,absurd)
import Data.Primitive (Array)

import Data.Index (Index)
import Data.Array.Indexed (Vector)
import qualified Data.Arithmetic.Types as Arithmetic
import qualified GHC.TypeNats as GHC

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

data Meta :: Nat -> Nat -> Nat -> Type where
  Meta ::
       Map () fh 'Zero
    -> Map (Meta fh ph mh) ph 'Zero
    -> Map (Meta fh ph mh) mh 'Zero
    -> Meta fh ph mh

type MetaEmpty = 'Meta 'MapEmpty 'MapEmpty 'MapEmpty
type MetaFields x = 'Meta x 'MapEmpty 'MapEmpty

-- A finite set. The first number is the total height (total possible
-- size is 2^h). The second number is how far down from the root
-- the node is. Common cases:
--
-- * n = 0: This node is the root node of a set.
-- * n = h: This node is a leaf node.
data Set (h :: Nat) (n :: Nat) where
  SetLeaf :: Set h h
  SetBranch :: Set h ('Succ n) -> Set h ('Succ n) -> Set h n
  SetLeft :: Set h ('Succ n) -> Set h n
  SetRight :: Set h ('Succ n) -> Set h n
  SetEmpty :: Set h 'Zero

data Map :: Type -> Nat -> Nat -> Type where
  MapLeaf :: forall (k :: Type) (h :: Nat).
       k
    -> Map k h h
  MapBranch ::
       Map k h ('Succ n)
    -> Map k h ('Succ n)
    -> Map k h n
  MapLeft :: Map k h ('Succ n) -> Map k h n
  MapRight :: Map k h ('Succ n) -> Map k h n
  MapEmpty :: Map k h 'Zero

data Elem :: forall (h :: Nat) (n :: Nat). Vec h Bool -> Vec n Bool -> Map () h n -> Type where
  ElemLeaf :: Elem @h @h v v ('MapLeaf '())
  ElemLeft ::
       Elem @h @('Succ n) v ('VecCons 'False w) sl 
    -> Elem @h @n v w ('MapLeft sl)
  ElemRight ::
       Elem @h @('Succ n) v ('VecCons 'True w) sr
    -> Elem @h @n v w ('MapRight sr)
  ElemBranchLeft ::
       Elem @h @('Succ n) v ('VecCons 'False w) sl 
    -> Elem @h @n v w ('MapBranch sl sr)
  ElemBranchRight ::
       Elem @h @('Succ n) v ('VecCons 'True w) sr
    -> Elem @h @n v w ('MapBranch sl sr)

-- First set is superset. Second one is subset.
data Subset (h :: Nat) (n :: Nat) (s :: Set h n) (r :: Set h n) where
  SubsetEmpty :: Subset h 'Zero s 'SetEmpty
  SubsetLeftLeft ::
       Subset h ('Succ n) s r
    -> Subset h n ('SetLeft s) ('SetLeft r)
  SubsetRightRight ::
       Subset h ('Succ n) s r
    -> Subset h n ('SetRight s) ('SetRight r)
  SubsetBranchLeft ::
       Subset h ('Succ n) sl r
    -> Subset h n ('SetBranch sl sr) ('SetLeft r)
  SubsetBranchRight ::
       Subset h ('Succ n) sr r
    -> Subset h n ('SetBranch sl sr) ('SetRight r)

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

newtype ApConst2 :: forall (a :: Type) (b :: Type). (b -> Type) -> a -> b -> Type where
  ApConst2 :: forall (a :: Type) (b :: Type) (x :: a) (y :: b) (f :: b -> Type).
    f y -> ApConst2 @a @b f x y

newtype ApConst1 :: forall (a :: Type) (b :: Type). (a -> Type) -> a -> b -> Type where
  ApConst1 :: forall (a :: Type) (b :: Type) (x :: a) (y :: b) (f :: a -> Type).
    f x -> ApConst1 @a @b f x y

data Collection ::
    forall (fh :: Nat). -- Field height
    forall (ph :: Nat). -- Prefix height
    forall (mh :: Nat). -- Multi height
    (GHC.Nat -> Vec fh Bool -> Type) -> -- Leaf Interpreter
    GHC.Nat ->
    Meta fh ph mh ->
    Type
  where
  Collection ::
       Arithmetic.Nat ch
    -> Vector par (Array (Index ch))
    -> Record i ch meta
    -> Collection i par meta

-- As we descend, the interpreter tweaks itself. Sort of. This
-- does not yet have any special treatment for 0-or-1 values,
-- but it could.
data Record ::
    forall (fh :: Nat) -- Field height
           (ph :: Nat) -- Prefix height
           (mh :: Nat). -- Multi height
    (GHC.Nat -> Vec fh Bool -> Type) -> -- Leaf Interpreter
    GHC.Nat -> -- Size, that is, number of elements per column
    Meta fh ph mh ->
    Type
  where
  Record :: forall
    (fh :: Nat) -- Field height
    (ph :: Nat) -- Prefix height
    (mh :: Nat) -- Multi height
    (i :: (GHC.Nat -> Vec fh Bool -> Type))
    (sz :: GHC.Nat) -- Size, that is, number of elements per column
    (f :: Map () fh 'Zero)
    (p :: Map (Meta fh ph mh) ph 'Zero)
    (m :: Map (Meta fh ph mh) mh 'Zero).
       Tree @fh @'Zero @() (ApConst2 @() @(Vec fh Bool) (i sz)) f 'VecNil
    -> Tree @ph @'Zero @(Meta fh ph mh)
         ( ApConst1
           @(Meta fh ph mh) @(Vec ph Bool)
           (Record @fh @ph @mh i sz)
         )
         p 'VecNil
    -> Tree @mh @'Zero @(Meta fh ph mh)
         ( ApConst1
           @(Meta fh ph mh) @(Vec mh Bool)
           (Collection @fh @ph @mh i sz)
         )
         m 'VecNil
    -> Record @fh @ph @mh i sz ('Meta f p m)

-- A balanced tree that always has a base-two number of leaves. The
-- type of the element at each position is determined by the interpretation
-- function applied to the finger into the tree (Vec h Bool).
data Tree ::
    forall (h :: Nat). -- Total height
    forall (n :: Nat).
    forall (k :: Type).
    (k -> Vec h Bool -> Type) -> -- Interpreter
    Map k h n -> -- Set of fields
    Vec n Bool -> -- Finger to position in set
    Type
  where
  TreeLeaf :: forall
       (h :: Nat)
       (k :: Type)
       (i :: k -> Vec h Bool -> Type)
       (v :: Vec h Bool)
       (a :: k)
     . i a v
    -> Tree @h @h i ('MapLeaf a) v
  TreeLeft ::
       Tree @h @('Succ n) i sl ('VecCons 'False v)
    -> Tree @h @n i ('MapLeft sl) v
  TreeRight ::
       Tree @h @('Succ n) @k i s ('VecCons 'True v)
    -> Tree @h @n @k i ('MapRight s) v
  TreeBranch ::
       Tree @h @('Succ n) @k i sl ('VecCons 'False v)
    -> Tree @h @('Succ n) @k i sr ('VecCons 'True v)
    -> Tree @h @n @k i ('MapBranch sl sr) v
  TreeEmpty :: Tree @h @'Zero @k i 'MapEmpty 'VecNil

-- This is like Tree except that every element is present. It is not
-- parameterized by a Set since everything is present, but each leaf
-- does keep a type-level finger to itself.
data Omnitree :: forall (h :: Nat) (n :: Nat).
    (Vec h Bool -> Type) ->
    Vec n Bool ->
    Type
  where
  OmnitreeLeaf ::
       i v
    -> Omnitree @h @h i v
  OmnitreeBranch ::
       Omnitree @h @('Succ n) i ('VecCons 'False v)
    -> Omnitree @h @('Succ n) i ('VecCons 'True v)
    -> Omnitree @h @n i v

-- The implementation is efficient. This function typically gets
-- used to build CAFs that last for a program's lifetime, so the
-- efficiency doesn't really matter.
omnibuild :: forall h i.
     SingNat h
  -> (forall v. Finger h v -> i v)
  -> Omnitree @h @'Zero i 'VecNil
omnibuild h0 f = go (gteZero h0) FingerNil where
  go :: forall m w. Gte h m -> Finger m w -> Omnitree @h @m i w
  go GteEq v = OmnitreeLeaf (f v)
  go (GteGt gt) v = OmnitreeBranch (go gt (FingerCons SingFalse v)) (go gt (FingerCons SingTrue v))

gteZero :: forall n. SingNat n -> Gte n 'Zero
gteZero = go GteEq where
  go :: forall m. Gte n m -> SingNat m -> Gte n 'Zero
  go gt SingZero = gt
  go gt (SingSucc x) = go (GteGt gt) x

-- singletonBase :: forall (h :: Nat) (v :: Vec h Bool) (s :: Set h h) (i :: Vec h Bool -> Type).
--   Finger h v -> i v -> Tree h 'Zero i (Singleton h h v ('SetLeaf 'True)) 'VecNil
-- singletonBase FingerNil e = TreeLeaf (Present e)
-- singletonBase (FingerCons b bs) e = _ (singletonBase bs e)

empty :: forall (h :: Nat) (k :: Type) (i :: k -> Vec h Bool -> Type).
  Tree @h @'Zero i 'MapEmpty 'VecNil
empty = TreeEmpty

-- emptyLeft :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: Set h ('Succ n)) (i :: Vec h Bool -> Type).
--      Tree h ('Succ n) i s ('VecCons 'False w)
--   -> Tree h ('Succ n) i (Empty h ('Succ n) s) ('VecCons 'True w)
-- emptyLeft (TreeLeaf _) = TreeLeaf Absent
-- emptyLeft (TreeBranch bl br) = TreeBranch (_ br) (_ br)
-- 
-- emptyTrue :: forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: Set h ('Succ n)) (i :: Vec h Bool -> Type).
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
singleton ::
     forall (h :: Nat) (n :: Nat) (k :: Type)
            (w :: Vec n Bool) (s :: Map k h n) (i :: k -> Vec h Bool -> Type).
     Finger n w -- Finger to node under consideration
  -> Tree @h @n i s w
  -> Tree @h @'Zero i (Singleton w s) 'VecNil
singleton FingerNil s = s
singleton (FingerCons b bs) s = case b of
  SingFalse -> singleton bs (TreeLeft s)
  SingTrue -> singleton bs (TreeRight s)

get :: Elem @h @n v w s -> Tree @h @n @() (ApConst2 i) s w -> i v
get ElemLeaf (TreeLeaf (ApConst2 x)) = x
get (ElemLeft e) (TreeLeft x) = get e x
get (ElemRight e) (TreeRight x) = get e x
get (ElemBranchLeft e) (TreeBranch x _) = get e x
get (ElemBranchRight e) (TreeBranch _ x) = get e x

-- omniget :: Finger h v -> Omnitree @h @n i v -> i v
-- omniget FingerNil (OmnitreeLeaf x) = x
-- omniget (FingerCons SingTrue e) (OmnitreeBranch x _) = omniget e x

-- omniget (ElemRight e) (TreeRight x) = get e x
-- omniget (ElemBranchLeft e) (TreeBranch x _) = get e x
-- omniget (ElemBranchRight e) (TreeBranch _ x) = get e x

map ::
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: Map () h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type).
     (forall (v :: Vec h Bool). Finger h v -> i v -> j v)
  -> Finger n w -- finger to the current nodes
  -> Tree @h @n (ApConst2 i) s w -- argument tree
  -> Tree @h @n (ApConst2 j) s w
map f !v (TreeLeaf (ApConst2 x)) = TreeLeaf (ApConst2 (f v x))
map _ !_ TreeEmpty = TreeEmpty
map f !v (TreeLeft a) = TreeLeft (map f (FingerCons SingFalse v) a)
map f !v (TreeRight b) = TreeRight (map f (FingerCons SingTrue v) b)
map f !v (TreeBranch a b) = TreeBranch (map f (FingerCons SingFalse v) a) (map f (FingerCons SingTrue v) b)

itraverse_ ::
     forall (h :: Nat) (n :: Nat) (k :: Type) (w :: Vec n Bool) (s :: Map k h n)
     (i :: k -> Vec h Bool -> Type) (m :: Type -> Type).
     Monad m
  => (forall (v :: Vec h Bool) (y :: k). Int -> Finger h v -> i y v -> m ())
  -> Int -- starting index
  -> Finger n w -- finger to the current nodes
  -> Tree @h @n @k i s w -- argument tree
  -> m Int
itraverse_ _ !ix !_ TreeEmpty = pure ix
itraverse_ f !ix !v (TreeLeaf x) = f ix v x $> (ix + 1)
itraverse_ f !ix !v (TreeLeft a) = itraverse_ f ix (FingerCons SingFalse v) a
itraverse_ f !ix !v (TreeRight b) = itraverse_ f ix (FingerCons SingTrue v) b
itraverse_ f !ix !v (TreeBranch a b) = do
  ix' <- itraverse_ f ix (FingerCons SingFalse v) a
  itraverse_ f ix' (FingerCons SingTrue v) b

traverse ::
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: Map () h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type) (m :: Type -> Type).
     Applicative m
  => (forall (v :: Vec h Bool). Finger h v -> i v -> m (j v))
  -> Finger n w -- finger to the current nodes
  -> Tree @h @n @() (ApConst2 i) s w -- argument tree
  -> m (Tree @h @n (ApConst2 j) s w)
traverse _ !_ TreeEmpty = pure TreeEmpty
traverse f !v (TreeLeaf (ApConst2 x)) = fmap (TreeLeaf . ApConst2) (f v x)
traverse f !v (TreeLeft a) = fmap TreeLeft (traverse f (FingerCons SingFalse v) a)
traverse f !v (TreeRight b) = fmap TreeRight (traverse f (FingerCons SingTrue v) b)
traverse f !v (TreeBranch a b) = liftA2 TreeBranch
  (traverse f (FingerCons SingFalse v) a)
  (traverse f (FingerCons SingTrue v) b)

traverseF ::
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: Map () h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type) (m :: Type -> Type).
     Applicative m
  => (forall (v :: Vec h Bool). i v -> m (j v))
  -> Tree @h @n @() (ApConst2 i) s w -- argument tree
  -> m (Tree @h @n @() (ApConst2 j) s w)
traverseF _ TreeEmpty = pure TreeEmpty
traverseF f (TreeLeaf (ApConst2 x)) = fmap (TreeLeaf . ApConst2) (f x)
traverseF f (TreeLeft a) = fmap TreeLeft (traverseF f a)
traverseF f (TreeRight b) = fmap TreeRight (traverseF f b)
traverseF f (TreeBranch a b) = liftA2 TreeBranch
  (traverseF f a)
  (traverseF f b)

-- foldMapRecord :: 
--      (forall (v :: Vec h Bool). Finger h v -> i v -> m)
--   -> Finger n w -- finger to the current nodes
--   -> Tree @h @n (ApConst2 i) s w -- argument tree
--   -> m
-- foldMapRecord f v

foldMap ::
     forall (h :: Nat) (n :: Nat) (k :: Type) (w :: Vec n Bool) (s :: Map k h n)
     (i :: k -> Vec h Bool -> Type) (m :: Type).
     Monoid m
  => (forall (v :: Vec h Bool) (y :: k). Finger h v -> i y v -> m)
  -> Finger n w -- finger to the current nodes
  -> Tree @h @n i s w -- argument tree
  -> m
foldMap _ !_ TreeEmpty = mempty
foldMap f !v (TreeLeaf x) = f v x
foldMap f !v (TreeLeft a) = foldMap f (FingerCons SingFalse v) a
foldMap f !v (TreeRight b) = foldMap f (FingerCons SingTrue v) b
foldMap f !v (TreeBranch a b) =
     foldMap f (FingerCons SingFalse v) a
  <> foldMap f (FingerCons SingTrue v) b

foldMapF ::
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (s :: Map () h n)
     (i :: Vec h Bool -> Type) (m :: Type).
     Monoid m
  => (forall (v :: Vec h Bool). i v -> m)
  -> Tree @h @n (ApConst2 i) s w -- argument tree
  -> m
foldMapF _ TreeEmpty = mempty
foldMapF f (TreeLeaf (ApConst2 x)) = f x
foldMapF f (TreeLeft a) = foldMapF f a
foldMapF f (TreeRight b) = foldMapF f b
foldMapF f (TreeBranch a b) = foldMapF f a <> foldMapF f b

-- Requires the two trees to match exactly. It is possible to provide
-- a variant that does not require an exact match. Such a function would
-- use Union in the result type.
zip :: forall (h :: Nat) (n :: Nat) (z :: Type) (w :: Vec n Bool) (r :: Map z h n)
     (i :: z -> Vec h Bool -> Type) (j :: z -> Vec h Bool -> Type)
     (k :: z -> Vec h Bool -> Type).
     (forall (v :: Vec h Bool) (y :: z). Finger h v -> i y v -> j y v -> k y v)
  -> Finger n w -- finger to the current nodes
  -> Tree @h @n @z i r w -- left tree
  -> Tree @h @n @z j r w -- right tree
  -> Tree @h @n @z k r w -- result tree
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
     forall (h :: Nat) (n :: Nat) (w :: Vec n Bool) (r :: Map () h n) (s :: Map () h n)
     (i :: Vec h Bool -> Type) (j :: Vec h Bool -> Type) (m :: Type -> Type).
     Applicative m
  => (forall (v :: Vec h Bool). Finger h v -> i v -> j v -> m ())
  -> Finger n w -- finger to the current nodes
  -> Tree @h @n (ApConst2 i) r w -- left tree
  -> Tree @h @n (ApConst2 j) s w -- right tree
  -> m ()
zipM_ _ !_ TreeEmpty _ = pure ()
zipM_ f !v (TreeLeaf (ApConst2 x)) (TreeLeaf (ApConst2 y)) = f v x y
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
zipM_ f !v (TreeBranch a1 a2) (TreeBranch b1 b2) =
     zipM_ f (FingerCons SingFalse v) a1 b1
  *> zipM_ f (FingerCons SingTrue v) a2 b2
zipM_ _ !_ (TreeBranch t _) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
zipM_ _ !_ (TreeBranch _ _) TreeEmpty = pure ()

-- Left-biased union.
leftUnion ::
  forall (h :: Nat) (n :: Nat) (k :: Type)
  (w :: Vec n Bool)
  (r :: Map k h n) (s :: Map k h n)
  (i :: k -> Vec h Bool -> Type).
     Tree @h @n @k i r w
  -> Tree @h @n @k i s w
  -> Tree @h @n @k i (Union r s) w
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

-- Recursive helper for unionRecord. This is very nearly a copy of
-- leftUnion and would be unnecessary if partially applied type
-- families existed.
unionPrefixMap ::
  forall (fh :: Nat) (ph :: Nat) (mh :: Nat) (n :: Nat) (sz :: GHC.Nat)
  (w :: Vec n Bool)
  (r :: Map (Meta fh ph mh) ph n) (s :: Map (Meta fh ph mh) ph n)
  (i :: GHC.Nat -> Vec fh Bool -> Type).
     Tree @ph @n (ApConst1 (Record i sz)) r w
  -> Tree @ph @n (ApConst1 (Record i sz)) s w
  -> Tree @ph @n (ApConst1 (Record i sz)) (UnionPrefix r s) w
unionPrefixMap TreeEmpty t = t
unionPrefixMap (TreeLeaf (ApConst1 x)) TreeEmpty = TreeLeaf (ApConst1 x)
unionPrefixMap (TreeLeaf (ApConst1 x)) (TreeLeaf (ApConst1 y)) = TreeLeaf (ApConst1 (unionRecord x y))
unionPrefixMap (TreeLeft x) (TreeLeft y) = TreeLeft (unionPrefixMap x y)
unionPrefixMap (TreeBranch xl xr) (TreeRight yr) = TreeBranch xl (unionPrefixMap xr yr)
unionPrefixMap (TreeBranch xl xr) (TreeLeft yl) = TreeBranch (unionPrefixMap xl yl) xr
unionPrefixMap (TreeBranch xl xr) (TreeBranch yl yr) = TreeBranch (unionPrefixMap xl yl) (unionPrefixMap xr yr)
unionPrefixMap (TreeLeft xl) (TreeBranch yl yr) = TreeBranch (unionPrefixMap xl yl) yr
unionPrefixMap (TreeRight xr) (TreeBranch yl yr) = TreeBranch yl (unionPrefixMap xr yr)
unionPrefixMap (TreeRight x) (TreeRight y) = TreeRight (unionPrefixMap x y)
unionPrefixMap (TreeLeaf _) (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
unionPrefixMap (TreeBranch t _) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
unionPrefixMap (TreeBranch xl xr) TreeEmpty = TreeBranch xl xr
unionPrefixMap (TreeLeft x) TreeEmpty = TreeLeft x
unionPrefixMap (TreeRight x) TreeEmpty = TreeRight x
unionPrefixMap (TreeLeft x) (TreeRight y) = TreeBranch x y
unionPrefixMap (TreeRight x) (TreeLeft y) = TreeBranch y x
unionPrefixMap (TreeLeft t) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
unionPrefixMap (TreeRight t) (TreeLeaf _) = absurd (impossibleGte (treeToGte t))
unionPrefixMap (TreeLeaf _) (TreeLeft t) = absurd (impossibleGte (treeToGte t))
unionPrefixMap (TreeLeaf _) (TreeRight t) = absurd (impossibleGte (treeToGte t))

unionRecord ::
  forall (h :: Nat) (n :: Nat) (p :: Nat)
  (ml :: Meta h n p) (mr :: Meta h n p) (sz :: GHC.Nat)
  (i :: GHC.Nat -> Vec h Bool -> Type).
     Record i sz ml
  -> Record i sz mr
  -> Record i sz (UnionMeta ml mr)
unionRecord (Record fl pl ml) (Record fr pr mr) =
  Record (leftUnion fl fr) (unionPrefixMap pl pr) (leftUnion ml mr)

iomnitraverse_ ::
     forall (h :: Nat) (n :: Nat) (k :: Type) (w :: Vec n Bool) (s :: Map k h n)
     (i :: k -> Vec h Bool -> Type) (j :: Vec h Bool -> Type) (m :: Type -> Type).
     Monad m
  => Omnitree @h @n j w
  -> (forall (v :: Vec h Bool) (y :: k). Int -> j v -> i y v -> m ())
  -> Int -- starting index
  -> Tree @h @n @k i s w -- argument tree
  -> m Int
iomnitraverse_ !_ _ !ix TreeEmpty = pure ix
iomnitraverse_ (OmnitreeLeaf r) f !ix (TreeLeaf x) = f ix r x $> (ix + 1)
iomnitraverse_ (OmnitreeLeaf _) _ !_ (TreeLeft t) = absurd (impossibleGte (treeToGte t))
iomnitraverse_ (OmnitreeLeaf _) _ !_ (TreeRight t) = absurd (impossibleGte (treeToGte t))
iomnitraverse_ (OmnitreeLeaf _) _ !_ (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
iomnitraverse_ (OmnitreeBranch x _) _ !_ (TreeLeaf _) =
  absurd (impossibleGte (omnitreeToGte x))
iomnitraverse_ (OmnitreeBranch x _) f !ix (TreeLeft a) = iomnitraverse_ x f ix a
iomnitraverse_ (OmnitreeBranch _ y) f !ix (TreeRight b) = iomnitraverse_ y f ix b
iomnitraverse_ (OmnitreeBranch x y) f !ix (TreeBranch a b) = do
  ix' <- iomnitraverse_ x f ix a
  iomnitraverse_ y f ix' b

-- Right fold over the values in a tree using an omnitree for
-- per-slot behavior.
omnifoldr ::
     Omnitree @h @n i v
  -> (forall (w :: Vec h Bool). i w -> j w -> b -> b)
  -> b
  -> Tree @h @n (ApConst2 j) s v
  -> b
omnifoldr (OmnitreeLeaf i) g b (TreeLeaf (ApConst2 j)) = g i j b
omnifoldr (OmnitreeLeaf _) _ _ (TreeLeft t) = absurd (impossibleGte (treeToGte t))
omnifoldr (OmnitreeLeaf _) _ _ (TreeRight t) = absurd (impossibleGte (treeToGte t))
omnifoldr (OmnitreeLeaf _) _ _ (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
omnifoldr (OmnitreeLeaf _) _ b TreeEmpty = b
omnifoldr (OmnitreeBranch x y) g b (TreeBranch p q) = omnifoldr x g (omnifoldr y g b q) p
omnifoldr (OmnitreeBranch x _) g b (TreeLeft p) = omnifoldr x g b p
omnifoldr (OmnitreeBranch _ y) g b (TreeRight q) = omnifoldr y g b q
omnifoldr (OmnitreeBranch x _) _ _ (TreeLeaf _) = absurd (impossibleGte (omnitreeToGte x))
omnifoldr (OmnitreeBranch _ _) _ b TreeEmpty = b

omnisubset ::
     Omnitree @h @n i v
  -> Tree @h @n (ApConst2 p) s v
  -> Tree @h @n (ApConst2 i) s v
omnisubset (OmnitreeLeaf i) (TreeLeaf _) = TreeLeaf (ApConst2 i)
omnisubset (OmnitreeLeaf _) (TreeLeft t) = absurd (impossibleGte (treeToGte t))
omnisubset (OmnitreeLeaf _) (TreeRight t) = absurd (impossibleGte (treeToGte t))
omnisubset (OmnitreeLeaf _) (TreeBranch t _) = absurd (impossibleGte (treeToGte t))
omnisubset (OmnitreeLeaf _) TreeEmpty = TreeEmpty
omnisubset (OmnitreeBranch x y) (TreeBranch p q) =
  TreeBranch (omnisubset x p) (omnisubset y q)
omnisubset (OmnitreeBranch x _) (TreeLeft p) = TreeLeft (omnisubset x p)
omnisubset (OmnitreeBranch _ y) (TreeRight q) = TreeRight (omnisubset y q)
omnisubset (OmnitreeBranch x _) (TreeLeaf _) = absurd (impossibleGte (omnitreeToGte x))
omnisubset (OmnitreeBranch _ _) TreeEmpty = TreeEmpty

impossibleGte :: forall (m :: Nat). Gte m ('Succ m) -> Void
impossibleGte (GteGt g) = impossibleGte (predGte g)

predGte :: Gte m ('Succ n) -> Gte m n
predGte GteEq = GteGt GteEq
predGte (GteGt g) = GteGt (predGte g)

treeToGte :: Tree @h @('Succ n) i s v -> Gte h ('Succ n)
treeToGte (TreeLeaf _) = GteEq
treeToGte (TreeBranch t _) = GteGt (treeToGte t)
treeToGte (TreeLeft t) = GteGt (treeToGte t)
treeToGte (TreeRight t) = GteGt (treeToGte t)

omnitreeToGte :: Omnitree @h @n i v -> Gte h n
omnitreeToGte (OmnitreeBranch t _) = GteGt (omnitreeToGte t)
omnitreeToGte (OmnitreeLeaf _) = GteEq

type family Empty (h :: Nat) (n :: Nat) (gt :: Gte h n) (v :: Vec n Bool) :: Set h n where
  Empty h h 'GteEq bs = 'SetLeaf
  
-- Construct a singleton set. One leaf is true and all others
-- are false.
-- TODO: Generalize this. It would not be hard.
type family Singleton (v :: Vec n Bool) (x :: Map k h n) :: Map k h 'Zero where
  Singleton 'VecNil s = s
  Singleton ('VecCons 'False v) s =
    Singleton v ('MapLeft s)
  Singleton ('VecCons 'True v) s =
    Singleton v ('MapRight s)

type family FieldList (xs :: [Vec h Bool]) :: Map () h 'Zero where
  FieldList '[] = 'MapEmpty
  FieldList (x ': xs) = Union (Singleton x ('MapLeaf '())) (FieldList xs)

type family UnionMeta (x :: Meta fh ph mh) (y :: Meta fh ph mh) :: Meta fh ph mh where
  UnionMeta ('Meta fl pl ml) ('Meta fr pr mr) =
    ('Meta (Union fl fr) (UnionPrefix pl pr) (Union ml mr))

-- UnionPrefix is essentially a duplicate of Union. Without
-- being able to partially apply type families, we have no
-- other good option. Notice that it is used for both standard
-- prefixes and multi-prefixes.
type family UnionPrefix (x :: Map (Meta fh ph mh) h n) (y :: Map (Meta fh ph mh) h n) :: Map (Meta fh ph mh) h n where
  UnionPrefix ('MapLeaf x) ('MapLeaf y) = 'MapLeaf (UnionMeta x y)
  UnionPrefix ('MapLeaf x) 'MapEmpty = 'MapLeaf x
  UnionPrefix 'MapEmpty s = s
  UnionPrefix ('MapLeft x) 'MapEmpty = 'MapLeft x
  UnionPrefix ('MapBranch xl xr) ('MapLeft yl) = 'MapBranch (UnionPrefix xl yl) xr
  UnionPrefix ('MapBranch xl xr) ('MapRight yr) = 'MapBranch xl (UnionPrefix xr yr)
  UnionPrefix ('MapLeft xl) ('MapBranch yl yr) = 'MapBranch (UnionPrefix xl yl) yr
  UnionPrefix ('MapRight xr) ('MapBranch yl yr) = 'MapBranch yl (UnionPrefix xr yr)
  UnionPrefix ('MapLeft x) ('MapLeft y) = 'MapLeft (UnionPrefix x y)
  UnionPrefix ('MapLeft x) ('MapRight y) = 'MapBranch x y
  UnionPrefix ('MapRight x) ('MapRight y) = 'MapRight (UnionPrefix x y)
  UnionPrefix ('MapRight x) ('MapLeft y) = 'MapBranch y x
  UnionPrefix ('MapRight x) 'MapEmpty = 'MapRight x
  UnionPrefix ('MapBranch xl xr) 'MapEmpty = 'MapBranch xl xr
  UnionPrefix ('MapBranch xl xr) ('MapBranch yl yr) = 'MapBranch (UnionPrefix xl yl) (UnionPrefix xr yr)

-- TODO: It may be necessary to rethink choice of which
-- unit data constructor to use.
type family Union (x :: Map k h n) (y :: Map k h n) :: Map k h n where
  Union ('MapLeaf x) ('MapLeaf _) = 'MapLeaf x
  Union ('MapLeaf x) 'MapEmpty = 'MapLeaf x
  Union 'MapEmpty s = s
  Union ('MapLeft x) 'MapEmpty = 'MapLeft x
  Union ('MapBranch xl xr) ('MapLeft yl) = 'MapBranch (Union xl yl) xr
  Union ('MapBranch xl xr) ('MapRight yr) = 'MapBranch xl (Union xr yr)
  Union ('MapLeft xl) ('MapBranch yl yr) = 'MapBranch (Union xl yl) yr
  Union ('MapRight xr) ('MapBranch yl yr) = 'MapBranch yl (Union xr yr)
  Union ('MapLeft x) ('MapLeft y) = 'MapLeft (Union x y)
  Union ('MapLeft x) ('MapRight y) = 'MapBranch x y
  Union ('MapRight x) ('MapRight y) = 'MapRight (Union x y)
  Union ('MapRight x) ('MapLeft y) = 'MapBranch y x
  Union ('MapRight x) 'MapEmpty = 'MapRight x
  Union ('MapBranch xl xr) 'MapEmpty = 'MapBranch xl xr
  Union ('MapBranch xl xr) ('MapBranch yl yr) = 'MapBranch (Union xl yl) (Union xr yr)
