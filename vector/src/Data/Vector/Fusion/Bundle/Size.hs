{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Data.Vector.Fusion.Bundle.Size
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
--
-- Size hints for streams.
--

module Data.Vector.Fusion.Bundle.Size (
  Size(..), clampedSubtract, smaller, smallerThan, larger, toMax, upperBound, lowerBound
) where

import Data.Vector.Fusion.Util ( delay_inline )

type SizeHint = Maybe Int

-- | Hint about size of size of buffer that will be produced.
data Size = Size
  { lowerBound :: !Int
  , upperBound :: !SizeHint
  }
  deriving( Eq, Show )

exact :: Int -> Size
exact n = Size n (Just n)

pattern Unknown = Size 0 Nothing


instance Num Size where
  Size lA uA + Size lB uB = Size (lA + lB) (sizeHintAdd uA uB)
    where
      sizeHintAdd Nothing  _        = Nothing
      sizeHintAdd _        Nothing  = Nothing
      sizeHintAdd (Just n) (Just m) = Just $! checkedAdd n m
  --
  Size lA uA - Size lB uB =
    Size (subtractLB lA uB) (subtractUB uA lB)
    where
      subtractLB n Nothing              = 0
      subtractLB n (Just m) | m >= n    = 0
                            | otherwise = n - m
      subtractUB Nothing  _ = Nothing
      subtractUB (Just n) m | m >= n    = Just 0
                            | otherwise = Just $! n - m
  --
  fromInteger = exact . fromInteger

  (*)    = error "vector: internal error * for Bundle.size isn't defined"
  abs    = error "vector: internal error abs for Bundle.size isn't defined"
  signum = error "vector: internal error signum for Bundle.size isn't defined"

checkedAdd :: Int -> Int -> Int
{-# INLINE checkedAdd #-}
checkedAdd m n
    -- Note: we assume m and n are >= 0.
  | r < m || r < n =
      error $ "Data.Vector.Fusion.Bundle.Size.checkedAdd: overflow: " ++ show r
  | otherwise = r
  where
    r = m + n


-- | Subtract two sizes with clamping to 0, for drop-like things
{-# INLINE clampedSubtract #-}
clampedSubtract :: Size -> Size -> Size
-- clampedSubtract (Exact m) (Exact n) = Exact (max 0 (m - n))
-- clampedSubtract (Max   m) (Exact n)
--   | m <= n = Exact 0
--   | otherwise = Max (m - n)
-- clampedSubtract (Exact m) (Max   _) = Max m
-- clampedSubtract (Max   m) (Max   _) = Max m
clampedSubtract _         _ = Unknown

-- | Minimum of two size hints
smaller :: Size -> Size -> Size
{-# INLINE smaller #-}
smaller (Size lA uA) (Size lB uB)
  = Size (min lA lB) (smallerU uA uB)
  where
    smallerU Nothing  n        = n
    smallerU n        Nothing  = n
    smallerU (Just n) (Just m) = Just $! min n m

-- | Maximum of two size hints
larger :: Size -> Size -> Size
{-# INLINE larger #-}
larger (Size lA uA) (Size lB uB)
  = Size (max lA lB) (largerU uA uB)
  where
    largerU Nothing  n        = Nothing
    largerU n        Nothing  = Nothing
    largerU (Just n) (Just m) = Just $! max n m



-- | Select a safe smaller than known size.
smallerThan :: Int -> Size -> Size
{-# INLINE smallerThan #-}
-- FIXME: ???
--
-- smallerThan m (Exact n) = Exact (delay_inline min m n)
-- smallerThan m (Max   n) = Max   (delay_inline min m n)
smallerThan _ Unknown   = Unknown



-- | Convert a size hint to an upper bound
toMax :: Size -> Size
-- toMax (Exact n) = Max n
-- toMax (Max   n) = Max n
toMax Unknown   = Unknown

