{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
module Test.Vector.TestData
  ( -- * TestData type class
    TestData(..)
  , P(..)
  , eq
  , Conclusion(..)
  , (===>)
    -- ** Writer
  , WriterT(..)
  , Writer
  , runWriter
  , hoistST
  , liftRunST
    -- * Helpers
  , limitUnfolds
  , limitUnfoldsM
  , notNull2
  , notNullS2
  , index_value_pairs
  , indices
  ) where

import Control.Monad.ST
import Control.Monad.Primitive
import qualified Control.Monad.Trans.Writer as W
import Test.QuickCheck
import Data.Functor.Identity
import Data.Bifunctor
import qualified Data.Vector               as V
import qualified Data.Vector.Strict        as VV
import qualified Data.Vector.Primitive     as VP
import qualified Data.Vector.Storable      as VS
import qualified Data.Vector.Unboxed       as VU
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Fusion.Bundle as S

import Test.Vector.Orphanage ()


-- | Type class which establishes isomorphism between value of type
--   @a@ and its model @Model a@.
class (Testable (EqTest a), Conclusion (EqTest a)) => TestData a where
  type Model a
  model :: a -> Model a
  unmodel :: Model a -> a
  -- | Data type used by QuickCheck for testing
  type EqTest a
  type instance EqTest a = Property
  -- | Compare two values for equality.
  equal :: a -> a -> EqTest a
  default equal :: (Eq a, EqTest a ~ Property) => a -> a -> EqTest a
  equal x y = property (x == y)


instance (Eq a, TestData a) => TestData (S.Bundle v a) where
  type Model (S.Bundle v a) = [Model a]
  model   = map model  . S.toList
  unmodel = S.fromList . map unmodel

instance (Eq a, TestData a) => TestData (V.Vector a) where
  type Model (V.Vector a) = [Model a]
  model   = map model  . V.toList
  unmodel = V.fromList . map unmodel

instance (Eq a, TestData a) => TestData (VV.Vector a) where
  type Model (VV.Vector a) = [Model a]
  model   = map model   . VV.toList
  unmodel = VV.fromList . map unmodel

instance (Eq a, VP.Prim a, TestData a) => TestData (VP.Vector a) where
  type Model (VP.Vector a) = [Model a]
  model   = map model   . VP.toList
  unmodel = VP.fromList . map unmodel

instance (Eq a, VS.Storable a, TestData a) => TestData (VS.Vector a) where
  type Model (VS.Vector a) = [Model a]
  model   = map model   . VS.toList
  unmodel = VS.fromList . map unmodel

instance (Eq a, VU.Unbox a, TestData a) => TestData (VU.Vector a) where
  type Model (VU.Vector a) = [Model a]
  model   = map model   . VU.toList
  unmodel = VU.fromList . map unmodel

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

-- | Newtype wrapper for QC property which compares value and it model
newtype P a = P { unP :: EqTest a }

instance TestData a => Testable (P a) where
  property (P a) = property a

-- | Compare value and model.
eq :: TestData a => a -> Model a -> P a
eq x y = P (equal x (unmodel y))
infix 4 `eq`

  
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


----------------------------------------------------------------
-- Writer wrapper
----------------------------------------------------------------

-- | We use writer as an example of a monad. In order to avoid orphans
--   it's defined as newtype wrapper over writer from @transformers@
newtype WriterT w m a = WriterT { runWriterT :: W.WriterT w m a }
  deriving (Show,Eq,Functor,Applicative,Monad,PrimMonad)

type Writer w = WriterT w Identity

runWriter :: Writer w a -> (a, w)
runWriter = W.runWriter . runWriterT

liftRunST :: (forall s. WriterT w (ST s) a) -> Writer w a
liftRunST m = WriterT $ W.WriterT $ Identity $ runST $ W.runWriterT $ runWriterT m

hoistST :: Writer w a -> WriterT w (ST s) a
hoistST = WriterT . W.WriterT . pure . runWriter


instance (Arbitrary w, Arbitrary a) => Arbitrary (Writer w a) where
  arbitrary = do w <- arbitrary
                 a <- arbitrary
                 return $ WriterT $ W.writer (w,a)

instance (CoArbitrary w) => CoArbitrary (Writer w ()) where
  coarbitrary = coarbitrary . W.runWriter . runWriterT

instance (Eq w, TestData w, Monoid w, Eq a, TestData a) => TestData (WriterT w Identity a) where
  type Model (WriterT w Identity a) = Writer (Model w) (Model a)
  model   = WriterT . W.mapWriter model   . runWriterT
  unmodel = WriterT . W.mapWriter unmodel . runWriterT

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Because the vectors are strict, we need to be totally sure that
--   the unfold eventually terminates. This is achieved by injecting
--   our own bit of state into the unfold - the maximum number of
--   unfolds allowed.
limitUnfolds :: (b       -> Maybe (a, b))
             -> (b, Int) -> Maybe (a, (b, Int))
limitUnfolds f (theirs, ours)
    | ours > 0
    , Just (out, theirs') <- f theirs = Just (out, (theirs', ours - 1))
    | otherwise                       = Nothing

limitUnfoldsM :: (Monad m)
              => (b       -> m (Maybe (a, b)))
              -> (b, Int) -> m (Maybe (a, (b, Int)))
limitUnfoldsM f (theirs, ours)
    | ours > 0 = do r <- f theirs
                    return $ (\(a,b) -> (a,(b,ours - 1))) `fmap` r
    | otherwise = return Nothing


notNull2 :: VG.Vector v a => x -> v a -> Bool
notNull2 _ xs = not $ VG.null xs
{-# INLINE notNull2 #-}

notNullS2 :: x -> S.Bundle v a -> Bool
notNullS2 _ s = not $ S.null s
{-# INLINE notNullS2 #-}

-- | Generate list of pair of index for vector of length @n@ and
--   arbitrary values
index_value_pairs :: Arbitrary a => Int -> Gen [(Int,a)]
index_value_pairs 0 = return []
index_value_pairs m = sized $ \n ->
  do
    len <- choose (0,n)
    is <- sequence [choose (0,m-1) | _i <- [1..len]]
    xs <- vector len
    return $ zip is xs

-- | Generate list of indices in range (0,n-1)
indices :: Int -> Gen [Int]
indices 0 = return []
indices m = sized $ \n ->
  do
    len <- choose (0,n)
    sequence [choose (0,m-1) | _i <- [1..len]]
