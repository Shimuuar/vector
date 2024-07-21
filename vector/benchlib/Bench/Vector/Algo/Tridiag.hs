module Bench.Vector.Algo.Tridiag ( tridiag, permute ) where

import Control.Monad
import Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

tridiag :: (Vector Double, Vector Double, Vector Double, Vector Double)
            -> Vector Double
{-# NOINLINE tridiag #-}
tridiag (as,bs,cs,ds) = V.prescanr' (\(c,d) x' -> d - c*x') 0
                      $ V.prescanl' modify (0,0)
                      $ V.zip (V.zip as bs) (V.zip cs ds)
    where
      modify (c',d') ((a,b),(c,d)) = 
                   let id = 1 / (b - c'*a)
                   in
                   id `seq` (c*id, (d-d'*a)*id)



permute :: Int -> Int -> IO (Vector Int)
permute sz n_it = do
  vec <- MV.generate sz id
  replicateM_ n_it $ MV.nextPermutation vec
  V.unsafeFreeze vec
