{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Data.Vector.Fusion.Bundle.Size
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : portable
--
-- Size hints for streams.
--

module Data.Vector.Fusion.Bundle.Size (
  Size(..), exact, unknown, maxSize,

  {-clampedSubtract,-} smaller, {-smallerThan,-} larger
  -- , toMax, upperBound, lowerBound
  , zeroLowerBound
  , setUpperBound
) where

import Data.Vector.Fusion.Util ( delay_inline )

-- | Hint about size of vector that will be produced from bundle. It's
--   an interval which bounds size of produced vector.
--
--   Upper bound is treated specially. @maxBound::Int@ means unbounded
--   stream and any estimate that overflows @Int@ is treated as
--   unbounded.
--
--   Note that it's possible to create vectors with length @maxBound@
--   since unboxed vectors for () use O(1) memory. So we won't run
--   out of memory first.
data Size = Size
  { lowerBound :: !Int
    -- ^ Lower bound on size of vector.
  , upperBound :: !Int
    -- ^ Upper bound on size of vector.
  }
  deriving( Eq, Show )

-- | Hint for size that is known exactly
exact :: Int -> Size
exact n = Size n n

-- | Unknown size
unknown :: Size
unknown = Size 0 maxBound

-- | Hint for case when we have upper bound but no lower bound
maxSize :: Int -> Size
maxSize n = Size 0 n

instance Num Size where
  Size lA uA + Size lB uB = Size (checkedAdd lA lB) (saturatedAdd uA uB)
  --
  Size lA uA - Size lB _
    | uA == maxBound = Size lR maxBound
    | otherwise      = Size lR (saturatedSub uA lB)
    where
      lR = saturatedSub lA lB
  --
  fromInteger = exact . fromInteger

  (*)    = error "vector: internal error * for Bundle.size isn't defined"
  abs    = error "vector: internal error abs for Bundle.size isn't defined"
  signum = error "vector: internal error signum for Bundle.size isn't defined"


-- | Add two non-negative integers and in case of overflow return maxBound
saturatedAdd :: Int -> Int -> Int
saturatedAdd a b | n < 0     = maxBound
                 | otherwise = n
  where n = a + b

-- | Subtract two non-negative integers. If result is negative it's set to zero
saturatedSub :: Int -> Int -> Int
saturatedSub a b | a < b     = 0
                 | otherwise = a - b


-- | Add two non-negative integers and throw error in case of overflow
checkedAdd :: Int -> Int -> Int
{-# INLINE checkedAdd #-}
checkedAdd m n
    -- Note: we assume m and n are >= 0.
  | r < m || r < n =
      error $ "Data.Vector.Fusion.Bundle.Size.checkedAdd: overflow: " ++ show r
  | otherwise = r
  where
    r = m + n


-- | Minimum of two size hints
smaller :: Size -> Size -> Size
{-# INLINE smaller #-}
smaller (Size lA uA) (Size lB uB)
  = Size (min lA lB) (min uA uB)


-- | Maximum of two size hints
larger :: Size -> Size -> Size
{-# INLINE larger #-}
larger (Size lA uA) (Size lB uB)
  = Size (max lA lB) (max uA uB)



-- | Set lower bound of size hint to zero
zeroLowerBound :: Size -> Size
{-# INLINE zeroLowerBound #-}
zeroLowerBound (Size _ ub) = Size 0 ub

-- | Set upper bound of size to given value
setUpperBound :: Int -> Size -> Size
{-# INLINE setUpperBound #-}
setUpperBound n (Size lb ub) = Size (min n lb) (min n ub)
