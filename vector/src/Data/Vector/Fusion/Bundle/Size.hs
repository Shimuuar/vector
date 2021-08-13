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
  Size(..), exact, unknown, maxSize,

  {-clampedSubtract,-} smaller, {-smallerThan,-} larger
  -- , toMax, upperBound, lowerBound
  , zeroLowerBound
  , setUpperBound
) where

import Data.Vector.Fusion.Util ( delay_inline )

type SizeHint = Maybe Int

-- | Hint about size of size of buffer that will be produced.
data Size = Size
  { lowerBound :: !Int
  , upperBound :: !SizeHint
  }
  deriving( Eq, Show )

-- | Hint for size that is known exactly
exact :: Int -> Size
exact n = Size n (Just n)

-- | Unknown size
unknown :: Size
unknown = Size 0 Nothing

-- | Maximum bound for size is known
maxSize :: Int -> Size
maxSize n = Size 0 (Just n)

instance Num Size where
  Size lA uA + Size lB uB = Size (checkedAdd lA lB) $ do
    a <- uA
    b <- uB
    saturatedAdd a b
  --
  Size lA uA - Size lB uB =
    Size (subtractLB lA uB) (subtractUB uA lB)
    where
      subtractLB _ Nothing              = 0
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

saturatedAdd :: Int -> Int -> Maybe Int
{-# INLINE saturatedAdd #-}
saturatedAdd m n
    -- Note: we assume m and n are >= 0.
  | r < m || r < n = Nothing
  | otherwise      = Just r
  where
    r = m + n

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
    largerU Nothing  _        = Nothing
    largerU _        Nothing  = Nothing
    largerU (Just n) (Just m) = Just $! max n m


-- | Set lower bound of size hint to zero
zeroLowerBound :: Size -> Size
{-# INLINE zeroLowerBound #-}
zeroLowerBound (Size _ ub) = Size 0 ub

-- | Set upper bound of size to given value
setUpperBound :: Int -> Size -> Size
{-# INLINE setUpperBound #-}
setUpperBound n (Size lb Nothing)   = Size (min lb n) (Just n)
setUpperBound n (Size lb (Just ub)) = Size (min lb n) (Just $! min n ub)
