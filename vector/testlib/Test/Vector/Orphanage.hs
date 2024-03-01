{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Orphan instances for vector data types
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Vector.Orphanage where

import Test.QuickCheck

import qualified Data.Vector.Fusion.Bundle as S
import qualified Data.Vector as V
import qualified Data.Vector.Strict as VV
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
 


instance Show a => Show (S.Bundle v a) where
    show s = "Data.Vector.Fusion.Bundle.fromList " ++ show (S.toList s)

instance Arbitrary a => Arbitrary (S.Bundle v a) where
    arbitrary = fmap S.fromList arbitrary

instance CoArbitrary a => CoArbitrary (S.Bundle v a) where
    coarbitrary = coarbitrary . S.toList


instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

instance CoArbitrary a => CoArbitrary (V.Vector a) where
    coarbitrary = coarbitrary . V.toList


instance Arbitrary a => Arbitrary (VV.Vector a) where
    arbitrary = fmap VV.fromList arbitrary

instance CoArbitrary a => CoArbitrary (VV.Vector a) where
    coarbitrary = coarbitrary . VV.toList


instance (Arbitrary a, VP.Prim a) => Arbitrary (VP.Vector a) where
    arbitrary = fmap VP.fromList arbitrary

instance (CoArbitrary a, VP.Prim a) => CoArbitrary (VP.Vector a) where
    coarbitrary = coarbitrary . VP.toList


instance (Arbitrary a, VS.Storable a) => Arbitrary (VS.Vector a) where
    arbitrary = fmap VS.fromList arbitrary

instance (CoArbitrary a, VS.Storable a) => CoArbitrary (VS.Vector a) where
    coarbitrary = coarbitrary . VS.toList


instance (Arbitrary a, VU.Unbox a) => Arbitrary (VU.Vector a) where
    arbitrary = fmap VU.fromList arbitrary

instance (CoArbitrary a, VU.Unbox a) => CoArbitrary (VU.Vector a) where
    coarbitrary = coarbitrary . VU.toList
