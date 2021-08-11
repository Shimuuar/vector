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

exact :: Int -> Size
exact n = Size n (Just n)

unknown :: Size
unknown = Size 0 Nothing

maxSize :: Int -> Size
maxSize n = Size 0 (Just n)

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
zeroLowerBound (Size _ ub) = Size 0 ub

setUpperBound :: Int -> Size -> Size
setUpperBound n (Size lb Nothing)   = Size (min lb n) (Just n)
setUpperBound n (Size lb (Just ub)) = Size (min lb n) (Just $! min n ub)
