{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Utilities where

import Test.QuickCheck
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Fusion.Bundle as S
import Control.Monad.Trans.Writer
import Test.Vector.TestData

instance (Arbitrary a, Arbitrary b) => Arbitrary (Writer a b) where
    arbitrary = do b <- arbitrary
                   a <- arbitrary
                   return $ writer (b,a)

instance CoArbitrary a => CoArbitrary (Writer a ()) where
    coarbitrary = coarbitrary . runWriter

instance (Eq a, TestData a, Eq b, TestData b, Monoid a) => TestData (Writer a b) where
  type Model (Writer a b) = Writer (Model a) (Model b)
  model = mapWriter model
  unmodel = mapWriter unmodel
