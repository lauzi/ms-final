{-# LANGUAGE PartialTypeSignatures #-}

module MyRandom where

import GHC.Arr (unsafeFreezeSTArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.ST (newListArray, readArray, writeArray)
import Data.Array.IArray (Array)
import System.Random (StdGen, randomR)
import Control.Monad (forM_)
import Control.Monad.ST (runST)

-- This makes arr's type messy so just use unsafeFreezeSTArray
-- (the optimized version of unsafeFreeze uses it anyways).
-- import Data.Array.Unsafe (unsafeFreeze)


-- https://wiki.haskell.org/Random_shuffle

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffleN :: Int -> StdGen -> (StdGen, Array Int Int)
shuffleN n gen = runST $ do
  g <- newSTRef gen
  let randomRST lohi = do
        (a, s') <- randomR lohi <$> readSTRef g
        writeSTRef g s'
        return a
  arr <- newListArray (0, n-1) [0..n-1]
  forM_ [0 .. n-2] $ \i -> do
    j <- randomRST (i, n-1)
    vi <- readArray arr i
    vj <- readArray arr j
    writeArray arr j vi
    writeArray arr i vj
  gen' <- readSTRef g
  arr' <- unsafeFreezeSTArray arr
  return (gen', arr')
