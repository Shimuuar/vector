{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Utilities where

import Test.QuickCheck

import Data.Bifunctor
import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Strict  as DVV
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.Fusion.Bundle as S

import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Test.Vector.Orphanage ()


instance (Arbitrary a, Arbitrary b) => Arbitrary (Writer a b) where
    arbitrary = do b <- arbitrary
                   a <- arbitrary
                   return $ writer (b,a)

instance CoArbitrary a => CoArbitrary (Writer a ()) where
    coarbitrary = coarbitrary . runWriter

class (Testable (EqTest a), Conclusion (EqTest a)) => TestData a where
  type Model a
  model :: a -> Model a
  unmodel :: Model a -> a

  type EqTest a
  type instance EqTest a = Property
  equal :: a -> a -> EqTest a
  default equal :: (Eq a, EqTest a ~ Property) => a -> a -> EqTest a
  equal x y = property (x == y)


instance (Eq a, TestData a) => TestData (S.Bundle v a) where
  type Model (S.Bundle v a) = [Model a]
  model   = map model  . S.toList
  unmodel = S.fromList . map unmodel

instance (Eq a, TestData a) => TestData (DV.Vector a) where
  type Model (DV.Vector a) = [Model a]
  model   = map model    . DV.toList
  unmodel = DV.fromList . map unmodel

instance (Eq a, DVP.Prim a, TestData a) => TestData (DVP.Vector a) where
  type Model (DVP.Vector a) = [Model a]
  model   = map model    . DVP.toList
  unmodel = DVP.fromList . map unmodel

instance (Eq a, DVS.Storable a, TestData a) => TestData (DVS.Vector a) where
  type Model (DVS.Vector a) = [Model a]
  model   = map model    . DVS.toList
  unmodel = DVS.fromList . map unmodel

instance (Eq a, TestData a) => TestData (DVV.Vector a) where
  type Model (DVV.Vector a) = [Model a]
  model   = map model    . DVV.toList
  unmodel = DVV.fromList . map unmodel

instance (Eq a, DVU.Unbox a, TestData a) => TestData (DVU.Vector a) where
  type Model (DVU.Vector a) = [Model a]
  model   = map model    . DVU.toList
  unmodel = DVU.fromList . map unmodel

#define id_TestData(ty) \
instance TestData ty where { \
  type Model ty = ty;        \
  model = id;                \
  unmodel = id }             \

id_TestData(())
id_TestData(Bool)
id_TestData(Int)
id_TestData(Ordering)

instance TestData Float where
  type Model Float = Float
  model = id
  unmodel = id

  equal x y = property (x == y || (isNaN x && isNaN y))

instance TestData Double where
  type Model Double = Double
  model = id
  unmodel = id

  equal x y = property (x == y || (isNaN x && isNaN y))

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance (Eq a, TestData a) => TestData (Maybe a) where
  type Model (Maybe a) = Maybe (Model a)
  model = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a, Eq b, TestData b) => TestData (Either a b) where
  type Model (Either a b) = Either (Model a) (Model b)
  model = bimap model model
  unmodel = bimap unmodel unmodel

instance (Eq a, TestData a) => TestData [a] where
  type Model [a] = [Model a]
  model = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a) => TestData (Identity a) where
  type Model (Identity a) = Identity (Model a)
  model = fmap model
  unmodel = fmap unmodel

instance (Eq a, TestData a, Eq b, TestData b, Monoid a) => TestData (Writer a b) where
  type Model (Writer a b) = Writer (Model a) (Model b)
  model = mapWriter model
  unmodel = mapWriter unmodel

instance (Eq a, Eq b, TestData a, TestData b) => TestData (a,b) where
  type Model (a,b) = (Model a, Model b)
  model (a,b) = (model a, model b)
  unmodel (a,b) = (unmodel a, unmodel b)

instance (Eq a, Eq b, Eq c, TestData a, TestData b, TestData c) => TestData (a,b,c) where
  type Model (a,b,c) = (Model a, Model b, Model c)
  model (a,b,c) = (model a, model b, model c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

instance (Arbitrary a, Show a, TestData a, TestData b) => TestData (a -> b) where
  type Model (a -> b) = Model a -> Model b
  model f = model . f . unmodel
  unmodel f = unmodel . f . model

  type EqTest (a -> b) = a -> EqTest b
  equal f g x = equal (f x) (g x)

newtype P a = P { unP :: EqTest a }

instance TestData a => Testable (P a) where
  property (P a) = property a

infix 4 `eq`
eq :: TestData a => a -> Model a -> P a
eq x y = P (equal x (unmodel y))

class Conclusion p where
  type Predicate p

  predicate :: Predicate p -> p -> p

instance Conclusion Property where
  type Predicate Property = Bool

  predicate = (==>)

instance Conclusion p => Conclusion (a -> p) where
  type Predicate (a -> p) = a -> Predicate p

  predicate f p = \x -> predicate (f x) (p x)

infixr 0 ===>
(===>) :: TestData a => Predicate (EqTest a) -> P a -> P a
p ===> P a = P (predicate p a)

notNull2 _ xs = not $ DVG.null xs
notNullS2 _ s = not $ S.null s

-- Generators
index_value_pairs :: Arbitrary a => Int -> Gen [(Int,a)]
index_value_pairs 0 = return []
index_value_pairs m = sized $ \n ->
  do
    len <- choose (0,n)
    is <- sequence [choose (0,m-1) | _i <- [1..len]]
    xs <- vector len
    return $ zip is xs

indices :: Int -> Gen [Int]
indices 0 = return []
indices m = sized $ \n ->
  do
    len <- choose (0,n)
    sequence [choose (0,m-1) | _i <- [1..len]]
