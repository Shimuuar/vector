-- |
-- Reference implementation based on lists for vector's functions.
module Test.Vector.Reference
  ( singleton
  , snoc
  , generate
  , generateM
  , slice
  , backpermute
  , prescanl
  , postscanl
  , prescanr
  , postscanr
  , accum
  , (//)
  , modifyList
  , writeList
  , imap
  , imapM
  , imapM_
  , izipWith
  , izipWithM
  , izipWithM_
  , izipWith3
  , ifilter
  , imapMaybe
  , spanR
  , breakR
  , ifoldl
  , iscanl
  , iscanr
  , ifoldr
  , ifoldM
  , ifoldrM
  , ifoldM_
  , minIndex
  , maxIndex
  , iterateNM
  , unfoldrM
  , limitUnfolds
  , limitUnfoldsM
  ) where

import Control.Arrow ((***))
import Control.Monad (foldM, foldM_, zipWithM, zipWithM_)
import Data.Foldable
import Data.List     (sortBy)
import Data.Maybe    (catMaybes)


singleton :: a -> [a]
singleton x = [x]

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

generate :: Int -> (Int -> a) -> [a]
generate n f = [f i | i <- [0 .. n-1]]

generateM :: Monad m => Int -> (Int -> m a) -> m [a]
generateM n f = sequence [f i | i <- [0 .. n-1]]

slice :: Int -> Int -> [a] -> [a]
slice i n xs = take n (drop i xs)

backpermute :: [a] -> [Int] -> [a]
backpermute xs is = map (xs!!) is

prescanl :: (b -> a -> b) -> b -> [a] -> [b]
prescanl f z = init . scanl f z

postscanl :: (b -> a -> b) -> b -> [a] -> [b]
postscanl f z = tail . scanl f z

prescanr :: (a -> b -> b) -> b -> [a] -> [b]
prescanr f z = tail . scanr f z

postscanr :: (a -> b -> b) -> b -> [a] -> [b]
postscanr f z = init . scanr f z

accum :: (a -> b -> a) -> [a] -> [(Int,b)] -> [a]
accum f xs ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (x:xxs) ((i,y) : pps) j
      | i == j     = go (f x y : xxs) pps j
    go (x:xxs) pps j = x : go xxs pps (j+1)
    go [] _ _      = []

(//) :: [a] -> [(Int, a)] -> [a]
xs // ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (_x:xxs) ((i,y) : pps) j
      | i == j     = go (y:xxs) pps j
    go (x:xxs) pps j = x : go xxs pps (j+1)
    go [] _ _      = []


withIndexFirst :: (((Int, a) -> r) -> [(Int, a)] -> q)
               -> ((Int -> a -> r) -> [a]       -> q)
withIndexFirst m f = m (uncurry f) . zip [0..]

modifyList :: [a] -> (a -> a) -> Int -> [a]
modifyList xs f i = zipWith merge xs (replicate i Nothing ++ [Just f] ++ repeat Nothing)
  where
    merge x Nothing  = x
    merge x (Just g) = g x

writeList :: [a] -> Int -> a -> [a]
writeList xs i a = modifyList xs (const a) i

imap :: (Int -> a -> a) -> [a] -> [a]
imap = withIndexFirst map

imapM :: Monad m => (Int -> a -> m a) -> [a] -> m [a]
imapM = withIndexFirst mapM

imapM_ :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM_ = withIndexFirst mapM_

izipWith :: (Int -> a -> a -> a) -> [a] -> [a] -> [a]
izipWith = withIndexFirst zipWith

izipWithM :: Monad m => (Int -> a -> a -> m a) -> [a] -> [a] -> m [a]
izipWithM = withIndexFirst zipWithM

izipWithM_ :: Monad m => (Int -> a -> a -> m b) -> [a] -> [a] -> m ()
izipWithM_ = withIndexFirst zipWithM_

izipWith3 :: (Int -> a -> a -> a -> a) -> [a] -> [a] -> [a] -> [a]
izipWith3 = withIndexFirst zipWith3

ifilter :: (Int -> a -> Bool) -> [a] -> [a]
ifilter f = map snd . withIndexFirst filter f

imapMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybe f = catMaybes . withIndexFirst map f

indexedLeftFold :: ((b -> (Int, a) -> c) -> t -> [(Int, a)] -> q)
                ->  (b -> Int -> a -> c) -> t -> [a]        -> q
indexedLeftFold fld f z = fld (uncurry . f) z . zip [0::Int ..]

spanR :: (a -> Bool) -> [a] -> ([a], [a])
spanR f = (reverse *** reverse) . span f . reverse

breakR :: (a -> Bool) -> [a] -> ([a], [a])
breakR f = (reverse *** reverse) . break f . reverse

ifoldl :: (a -> Int -> a -> a) -> a -> [a] -> a
ifoldl = indexedLeftFold foldl

iscanl :: (Int -> a -> b -> a) -> a -> [b] -> [a]
iscanl f z = scanl (\a (i, b) -> f i a b) z . zip [0..]

iscanr :: (Int -> a -> b -> b) -> b -> [a] -> [b]
iscanr f z = scanr (uncurry f) z . zip [0..]

ifoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr f z = foldr (uncurry f) z . zip [0..]

ifoldM :: Monad m => (b -> Int -> a -> m b) -> b -> [a] -> m b
ifoldM = indexedLeftFold foldM

ifoldrM :: Monad m => (Int -> a -> b -> m b) -> b -> [a] -> m b
ifoldrM f z xs = foldrM (\(i,a) b -> f i a b) z ([0..] `zip` xs)

ifoldM_ :: Monad m => (b -> Int -> a -> m b) -> b -> [a] -> m ()
ifoldM_ = indexedLeftFold foldM_

minIndex :: Ord a => [a] -> Int
minIndex = fst . foldr1 imin . zip [0..]
  where
    imin (i,x) (j,y) | x <= y    = (i,x)
                     | otherwise = (j,y)

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . foldr1 imax . zip [0..]
  where
    imax (i,x) (j,y) | x >= y    = (i,x)
                     | otherwise = (j,y)

iterateNM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateNM n f x
    | n <= 0    = return []
    | n == 1    = return [x]
    | otherwise =  do x' <- f x
                      xs <- iterateNM (n-1) f x'
                      return (x : xs)

unfoldrM :: Monad m => (b -> m (Maybe (a,b))) -> b -> m [a]
unfoldrM step b0 = do
    r <- step b0
    case r of
      Nothing    -> return []
      Just (a,b) -> do as <- unfoldrM step b
                       return (a : as)

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
