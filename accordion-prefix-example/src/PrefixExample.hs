{-# language DataKinds #-}
{-# language EmptyCase #-}
{-# language PolyKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module PrefixExample where

import Data.Functor.Identity (Identity(Identity))
import Control.Monad.ST (runST)
import Accordion.Types
import Data.Type.Equality ((:~:)(Refl))
import Data.Kind (Type)
import Accordion.World (World,SingWorld)
import Accordion.Nat (N2,N1,N0)
import Vector.Boxed (BoxedVector)
import Vector.Unboxed (UnboxedVector)
import GHC.TypeNats (type (+))
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)

import qualified Accordion.World as W
import qualified Accordion.Types as A
import qualified GHC.TypeNats as GHC
import qualified Data.Array.Indexed as V
import qualified Data.Arithmetic.Nat as Nat
import qualified Data.Index as Index
import qualified Data.Primitive as PM
import qualified Data.Arithmetic.Types as Arithmetic

age' :: Int -> Tree
  (ApConst2 (Interpreted Identity))
  (FieldList '[ Index 'Age] )
  'VecNil
age' i = singleton (index SingAge) (index SingAge)
  (TreeLeaf (ApConst2 (Interpreted (Identity i))))

alive :: Bool -> MyBlade (MetaFields (FieldList '[Alive]))
alive i = MyBlade $ Blade
  ( singleton (index SingAlive) (index SingAlive)
    (TreeLeaf (ApConst2 (AsVec (V.singleton i))))
  )
  A.TreeEmpty
  A.TreeEmpty

age :: Int -> MyBlade (MetaFields (FieldList '[Age]))
age i = MyBlade $ Blade
  ( singleton (index SingAge) (index SingAge)
    (TreeLeaf (ApConst2 (AsVec (V.singleton i))))
  )
  A.TreeEmpty
  A.TreeEmpty

dogs ::
     Blades m
  -> MyBlade ('Meta 'MapEmpty 'MapEmpty (Singleton Dogs ('MapLeaf m)))
dogs (Blades n x) = MyBlade $ Blade TreeEmpty TreeEmpty
  (A.singleton (indexMultiprefix SingDogs) (indexMultiprefix SingDogs)
    (TreeLeaf (ApConst1 (Collection n (V.singleton (ascendingArray n)) x)))
  )

dog :: Record f m
  -> Record f ('Meta 'MapEmpty (Singleton (IndexPrefix 'Dog) ('MapLeaf m)) 'MapEmpty)
dog r = Record TreeEmpty
  (singleton
    (indexPrefix SingDog)
    (indexPrefix SingDog)
    (TreeLeaf (ApConst1 r))
  )

union :: MyBlade r -> MyBlade s -> MyBlade (UnionMeta r s)
union (MyBlade x) (MyBlade y) = MyBlade (A.unionBlade x y)

data Field
  = Age
  | Health
  | Letter
  | Alive

data Prefix
  = Dog
  | Cat

data Multiprefix
  = Dogs
  | Cats

data SingField :: Field -> Type where
  SingAge :: SingField 'Age
  SingHealth :: SingField 'Health
  SingLetter :: SingField 'Letter
  SingAlive :: SingField 'Alive

deriving instance Show (SingField d)

data SingPrefix :: Prefix -> Type where
  SingDog :: SingPrefix 'Dog
  SingCat :: SingPrefix 'Cat

data SingMultiprefix :: Multiprefix -> Type where
  SingDogs :: SingMultiprefix 'Dogs
  SingCats :: SingMultiprefix 'Cats

data Universe
  = Number
  | Character
  | Boolean

data SingUniverse :: Universe -> Type where
  SingNumber :: SingUniverse 'Number
  SingCharacter :: SingUniverse 'Character
  SingBoolean :: SingUniverse 'Boolean

type family Unindex (v :: Vec N2 Bool) :: Field where
  Unindex ('VecCons 'True ('VecCons 'True 'VecNil)) = 'Age
  Unindex ('VecCons 'False ('VecCons 'True 'VecNil)) = 'Health
  Unindex ('VecCons 'True ('VecCons 'False 'VecNil)) = 'Letter
  Unindex ('VecCons 'False ('VecCons 'False 'VecNil)) = 'Alive

type family Index (d :: Field) :: Vec N2 Bool where
  Index 'Age = 'VecCons 'True ('VecCons 'True 'VecNil)
  Index 'Health = 'VecCons 'False ('VecCons 'True 'VecNil)
  Index 'Letter = 'VecCons 'True ('VecCons 'False 'VecNil)
  Index 'Alive = 'VecCons 'False ('VecCons 'False 'VecNil)

type family IndexPrefix (p :: Prefix) :: Vec N1 Bool where
  IndexPrefix 'Dog = 'VecCons 'True 'VecNil
  IndexPrefix 'Cat = 'VecCons 'False 'VecNil

type family IndexMulti (m :: Multiprefix) :: Vec N1 Bool where
  IndexMulti 'Dogs = 'VecCons 'True 'VecNil
  IndexMulti 'Cats = 'VecCons 'False 'VecNil

type Alive = Index 'Alive
type Age = Index 'Age
type Dogs = IndexMulti 'Dogs
type Cats = IndexMulti 'Cats

newtype Interpreted :: (Type -> Type) -> Vec N2 Bool -> Type where
  Interpreted :: f (Ground (Interpret v)) -> Interpreted f v

type family Interpret (v :: Vec N2 Bool) :: Universe where
  Interpret x = InterpretField (Unindex x)

type family InterpretField (d :: Field) :: Universe where
  InterpretField 'Age = 'Number
  InterpretField 'Health = 'Number
  InterpretField 'Alive = 'Boolean
  InterpretField 'Letter = 'Character

type family Represent (u :: Universe) :: World Extra where
  Represent 'Number = ('W.Primitive 'W.Int)
  Represent 'Boolean = ('W.Primitive 'W.Bool)
  Represent 'Character = ('W.Primitive 'W.Char)

type family Ground (u :: Universe) :: Type where
  Ground 'Number = Int
  Ground 'Boolean = Bool
  Ground 'Character = Char

showIndexField :: Finger N2 v -> String
showIndexField = show . unindex

unindex :: Finger N2 v -> SingField (Unindex v)
unindex (FingerCons SingTrue (FingerCons SingTrue FingerNil)) = SingAge
unindex (FingerCons SingFalse (FingerCons SingTrue FingerNil)) = SingHealth
unindex (FingerCons SingTrue (FingerCons SingFalse FingerNil)) = SingLetter
unindex (FingerCons SingFalse (FingerCons SingFalse FingerNil)) = SingAlive

index :: SingField d -> Finger N2 (Index d)
index SingAge = (FingerCons SingTrue (FingerCons SingTrue FingerNil))
index SingHealth = (FingerCons SingFalse (FingerCons SingTrue FingerNil))
index SingLetter = (FingerCons SingTrue (FingerCons SingFalse FingerNil))
index SingAlive = (FingerCons SingFalse (FingerCons SingFalse FingerNil))

indexPrefix :: SingPrefix p -> Finger N1 (IndexPrefix p)
indexPrefix SingDog = FingerCons SingTrue FingerNil
indexPrefix SingCat = FingerCons SingFalse FingerNil

indexMultiprefix :: SingMultiprefix p -> Finger N1 (IndexMulti p)
indexMultiprefix SingDogs = FingerCons SingTrue FingerNil
indexMultiprefix SingCats = FingerCons SingFalse FingerNil

interpret :: Finger N2 v -> SingUniverse (Interpret v)
interpret f = case unindex f of
  SingAge -> SingNumber
  SingHealth -> SingNumber
  SingAlive -> SingBoolean
  SingLetter -> SingCharacter

represent :: SingUniverse u -> SingWorld SingExtra (Represent u)
represent SingNumber = W.SingPrimitive W.SingInt
represent SingCharacter = W.SingPrimitive W.SingChar
represent SingBoolean = W.SingPrimitive W.SingBool

indexRoundTrip :: SingField d -> (Unindex (Index d) :~: d)
indexRoundTrip SingAge = Refl
indexRoundTrip SingHealth = Refl
indexRoundTrip SingAlive = Refl
indexRoundTrip SingLetter = Refl

data Extra

data SingExtra :: Extra -> Type where

type family AsVecFam (n :: GHC.Nat) (v :: Vec N2 Bool) :: Type where
  AsVecFam n v = V.Vector n (Ground (Interpret v))

newtype AsVec :: GHC.Nat -> Vec N2 Bool -> Type where
  AsVec :: AsVecFam n v -> AsVec n v

newtype MyBlade :: Meta N2 N1 N1 -> Type where
  MyBlade :: Blade @N2 @N1 @N1 AsVec 1 m -> MyBlade m

data Blades :: Meta N2 N1 N1 -> Type where
  Blades ::
       Arithmetic.Nat n
    -> Blade @N2 @N1 @N1 AsVec n m
    -> Blades m

instance Semigroup (Blades m) where
  Blades x a <> Blades y b = Blades (Nat.plus x y)
    (getMyBlades (appendMyBlades (MyBlades a) (MyBlades b)))

newtype MyBlades :: GHC.Nat -> Meta N2 N1 N1 -> Type where
  MyBlades ::
    { getMyBlades :: Blade @N2 @N1 @N1 AsVec n m
    } -> MyBlades n m

concatBlades :: NonEmpty (MyBlade m) -> Blades m
concatBlades = sconcat . fmap singletonBlades

singletonBlades :: MyBlade m -> Blades m
singletonBlades (MyBlade m) = case m of
  Blade x y z -> Blades Nat.one (Blade x y z)

appendMyBlades :: forall na nb m.
  MyBlades na m -> MyBlades nb m -> MyBlades (na + nb) m
appendMyBlades
  (MyBlades (Blade b1 c1 d1))
  (MyBlades (Blade b2 c2 d2)) = MyBlades $ Blade @N2 @N1 @N1 @AsVec
    (A.zip (\_ (ApConst2 (AsVec v1)) (ApConst2 (AsVec v2)) ->
      ApConst2 (AsVec (V.append v1 v2))) FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $ getMyBlades $
      appendMyBlades (MyBlades p1) (MyBlades p2)) FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) FingerNil d1 d2
    )

appendCollections :: forall na nb (m :: Meta N2 N1 N1).
  Collection AsVec na m -> Collection AsVec nb m -> Collection AsVec (na + nb) m
appendCollections
  (Collection x1 a1 (Blade b1 c1 d1))
  (Collection x2 a2 (Blade b2 c2 d2)) =
  let sza1 = V.length a1
      sza2 = V.length a2
  in
  Collection (Nat.plus x1 x2)
    ( V.append
      (fmap (fmap (Index.incrementLimitR x2)) a1)
      (fmap (fmap (\i -> Index.incrementL x1 i)) a2)
    ) $ Blade @N2 @N1 @N1 @AsVec
    (A.zip (\_ (ApConst2 (AsVec v1)) (ApConst2 (AsVec v2)) ->
      ApConst2 (AsVec (V.append v1 v2))) FingerNil b1 b2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $ getMyBlades $
      appendMyBlades (MyBlades p1) (MyBlades p2)) FingerNil c1 c2
    )
    (A.zip (\_ (ApConst1 p1) (ApConst1 p2) -> ApConst1 $
      appendCollections p1 p2) FingerNil d1 d2
    )

singletonArray :: a -> PM.Array a
singletonArray x = runST
  (PM.newArray 1 x >>= PM.unsafeFreezeArray)

ascendingArray :: Arithmetic.Nat n -> PM.Array (Index.Index n)
ascendingArray n = V.forget $ runST $ do
  m <- V.new n
  Index.ascendM (\ix@(Index.Index lt p) -> V.write lt m p ix) n
  V.unsafeFreeze m

