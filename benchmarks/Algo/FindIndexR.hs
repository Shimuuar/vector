module Algo.FindIndexR (findIndexR, findIndexR_naive, findIndexR_manual)
where

import Foreign.C.Types (CDouble)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Generic as V

findIndexR :: (CDouble -> Bool, Vector CDouble) -> Maybe Int
{-# NOINLINE findIndexR #-}
findIndexR = uncurry V.findIndexR

findIndexR_naive :: (CDouble -> Bool, Vector CDouble) -> Maybe Int
{-# NOINLINE findIndexR_naive #-}
findIndexR_naive (pred, v) = fmap (V.length v - 1 -)
    $ V.foldl (\a x -> if pred x
                        then Just 1
                        else succ<$>a) Nothing v

findIndexR_manual :: (CDouble -> Bool, Vector CDouble) -> Maybe Int
{-# NOINLINE findIndexR_manual #-}
findIndexR_manual (pred, v) = go $ V.length v - 1
 where go i | i < 0                     = Nothing
            | pred (V.unsafeIndex v i)  = Just i
            | otherwise                 = go $ i-1

