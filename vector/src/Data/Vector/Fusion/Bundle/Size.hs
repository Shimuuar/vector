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
  Size(..), lowerBound, upperBound, exact, unknown, maxSize,

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
data Size = Size  !Int !Int
          | Exact !Int
          deriving( Eq, Show )

-- | Lower bound on size of vector.
lowerBound :: Size -> Int
{-# INLINE lowerBound #-}
lowerBound (Size  n _) = n
lowerBound (Exact n)   = n

-- ^ Upper bound on size of vector.
upperBound :: Size -> Int
{-# INLINE upperBound #-}
upperBound (Size  _ n) = n
upperBound (Exact n)   = n


-- | Hint for size that is known exactly
exact :: Int -> Size
exact = Exact

-- | Unknown size
unknown :: Size
unknown = Size 0 maxBound

-- | Hint for case when we have upper bound but no lower bound
maxSize :: Int -> Size
maxSize n = Size 0 n

instance Num Size where
  Exact nA   + Exact nB   = Exact (checkedAdd nA nB)
  Exact nA   + Size lB uB = Size (checkedAdd nA lB) (saturatedAdd nA uB)
  Size lA uA + Exact nB   = Size (checkedAdd lA nB) (saturatedAdd uA nB)
  Size lA uA + Size lB uB = Size (checkedAdd lA lB) (saturatedAdd uA uB)
  --
  Exact nA   - Exact nB   = Exact (saturatedSub nA nB)
  Exact nA   - Size lB uB = Size  (saturatedSub nA uB) (saturatedSub nA lB)
  Size lA uA - sz
    | uA == maxBound = Size lR maxBound
    | otherwise      = Size lR uR
    where
      lR = saturatedSub lA (upperBound sz)
      uR = saturatedSub uA (lowerBound sz)
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
smaller (Exact nA) (Exact nB) = Exact (min nA nB)
smaller szA szB = 
  Size (lowerBound szA `min` lowerBound szB) (upperBound szA `min` upperBound szB)


-- | Maximum of two size hints
larger :: Size -> Size -> Size
{-# INLINE larger #-}
larger (Exact nA) (Exact nB) = Exact (max nA nB)
larger szA szB =
  Size (lowerBound szA `max` lowerBound szB) (upperBound szA `max` upperBound szB)




-- | Set lower bound of size hint to zero
zeroLowerBound :: Size -> Size
{-# INLINE zeroLowerBound #-}
zeroLowerBound sz = Size 0 (upperBound sz)

-- | Set upper bound of size to given value
setUpperBound :: Int -> Size -> Size
{-# INLINE setUpperBound #-}
setUpperBound n (Exact k) = Exact (min n k)
setUpperBound n (Size lb ub) = Size (min n lb) (min n ub)
