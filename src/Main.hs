{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.List (mapAccumL, intercalate)
import Data.Tuple (swap)
import Data.Array.IArray
import Text.Printf
import System.Random (StdGen, mkStdGen)
import Control.Monad

import Image
import MyRandom (shuffleN)

import Debug.Trace

type GradMat = Array (Int, Int) (Int, Int)

calcGradGAP :: Image -> (GradMat, IntMat)
calcGradGAP (Image h w pxls) =
  (listArray bounds' grads, listArray bounds' gaps)
  where
    (grads, gaps) = unzip $ map gradGAP (range bounds')
    bounds' = ((1, 1), (h, w))

    get p
      | not (inRange bounds' p) = 0
      | otherwise = pxls ! p

    gradGAP (i, j) = ((dv, dh), gap)
      where
        get' (x, y) = get (x+i, y+j)

        [inn, inne, inw, in', ine, iww, iw] = map get' neighbors
        dv = sum $ map abs [iw - inw, in' - inn, ine - inne]
        dh = sum $ map abs [iw - iww, in' - inw, in' -  ine]
        dvh = dv - dh
        ic = (iw + in') `div` 2 + (ine - inw) `div` 4

        gap
          | dvh <= 8 && dvh >= -8 = ic
          | dvh >  80 = iw
          | dvh < -80 = in'
          | dvh >  32 = (ic +  iw) `div` 2
          | dvh < -32 = (ic + in') `div` 2
          | dvh >   8 = (3 * ic +  iw) `div` 4
          | dvh <  -8 = (3 * ic + in') `div` 4
          | otherwise = error "calcGradGAP.gap: GGWP"

neighbors :: [(Int, Int)]
neighbors = [           (-2, 0), (-2, 1),
              (-1, -1), (-1, 0), (-1, 1),
     (0, -2), ( 0, -1)]

predictionErrors :: Image -> IntMat -> IntMat
predictionErrors (Image h w pxls) gap =
  listArray bounds' $ zipWith (-) origs preds
  where
    bounds' = ((1, 1), (h, w))
    origs = elems pxls
    preds = elems gap

calcErrorEnergy :: GradMat -> IntMat -> IntMat
calcErrorEnergy grads errs =
  listArray bounds' $ map ee (range bounds')
  where
    bounds' = bounds grads

    getError p@(_, j)
      | j <= 0 = 0
      | otherwise = errs ! p

    ee (i, j) = dh + dv + 2 * abs (getError (i, j-1))
      where (dh, dv) = grads ! (i, j)

-- imagePixel -> gapPrediction -> mappedError
mappedError :: Int -> Int -> Int
mappedError i i'
  | i' >= 128 = mappedError (255-i) (255-i')
  |   e <=  0 = 2 * negate e
  |   e <= i' = 2 * e - 1
  | otherwise = e + i'
  where e = i - i'

-- Definition: unmapError (mappedError i i') = i - i'
-- When i' >= 128,
--      e' = mappedError i i' = mappedError (255-i) (255-i'),
--      unmapError (mappedError (255-i) (255-i')) (255-i') = (255-i) - (255-i')
--   => unmapError e' (255-i') = i' - i
--   => unmapError e' i' = i - i' = negate $ unmapError e' (255-i').
unmapError :: Int -> Int -> Int
unmapError e' i'
  | i' >= 128   = negate $ unmapError e' (255-i')
  | e' > 2 * i' = e' - i'
  | even e'     = negate (e' `div` 2)
  | otherwise   = (e'+1) `div` 2

remappedErrors :: Image -> IntMat -> IntMat
remappedErrors (Image h w pxls) predMat =
  listArray bounds' $ zipWith mappedError origs preds
  where
    bounds' = ((1, 1), (h, w))
    origs = elems pxls
    preds = elems predMat


restorePixels :: (GradMat, IntMat) -> Array Int [Int] -> IntMat
restorePixels (grads, preds) initArr =
  listArray bounds' $ zipWith (+) (elems preds) errs
  where
    bounds' = bounds grads
    ee = calcErrorEnergy grads $ listArray bounds' errs
    errs = go (elems initArr) (range bounds')

    q = quantizer defaultBins

    go _ [] = []
    go arr (p:xs) = e : go arr' xs
      where
        i' = preds ! p
        qv = q (ee ! p)
        e' = head (arr !! qv)
        e = unmapError e' i'
        arr' = popNthBin qv arr

popNthBin :: Int -> [[a]] -> [[a]]
popNthBin n xss =
  let (yss, (_:xs) : zss) = splitAt n xss
  in yss ++ (xs : zss)


type Block = Array (Int, Int) Int
partitionErrors :: Quantizer -> IntMat -> IntMat -> [Block]
partitionErrors q errArr eeArr =
  map makeBlock . unpack . accumErrors $ zip qees errs
  where
    bounds' = (0, q 2047)
    accumErrors = accumArray ((. (:)) . (.)) id bounds' :: _ -> Array _ _
    unpack = map ($ [0, 0, 0]) . elems

    makeBlock xs = listArray ((0, 0), (3, length xs `div` 4 - 1)) xs

    qees = map q (elems eeArr)
    errs = elems errArr


type Quantizer = Int -> Int
type Bins = [Int]

quantizationLevels :: Int
quantizationLevels = 8

quantizer :: Bins -> Quantizer
quantizer = (!) . (makeArray . expand 0)
  where
    makeArray = listArray (0, 2047) :: [Int] -> Array Int Int
    expand v [] = repeat v
    expand v (n:ns) = replicate n v ++ expand (v+1) (map (subtract n) ns)

defaultBins :: Bins
defaultBins = [5, 15, 25, 42, 60, 85, 140]


rearrange :: Array Int Int -> Array Int Int -> Block -> Block
rearrange xs ys blk = ixmap (bounds blk) f blk
  where f (x, y) = (xs ! x, ys ! y)

shuffleBlock, deshuffleBlock :: StdGen -> Block -> (StdGen, Block)
shuffleBlock = arrangeBlock id
deshuffleBlock = arrangeBlock inverse
  where inverse arr = array (bounds arr) (map swap (assocs arr))

arrangeBlock :: (Array Int Int -> Array Int Int) ->
                StdGen -> Block -> (StdGen, Block)
arrangeBlock f g blk = (g'', blk')
  where
    (g' , xs) = f <$> shuffleN 4 g
    (g'', ys) = f <$> shuffleN (succ . snd . snd $ bounds blk) g'
    blk' = rearrange xs ys blk

shuffleBlocks :: StdGen -> [Block] -> (StdGen, [Block])
shuffleBlocks = mapAccumL shuffleBlock


blocksToFile :: [Block] -> [Int]
blocksToFile = concatMap cm
  where cm blk = (succ . snd . snd . bounds $ blk) : elems blk


toFile :: StdGen -> Image -> [Int]
toFile g img = imgHeight img : imgWidth img : blocksToFile blocks'
  where
    (grads, preds) = calcGradGAP img
    ees = calcErrorEnergy grads (predictionErrors img preds)
    err's = remappedErrors img preds
    blocks = partitionErrors (quantizer defaultBins) err's ees
    blocks' = snd $ shuffleBlocks g blocks

fromFile :: StdGen -> [Int] -> Image
fromFile g (h:w:xs) = buildImage h w $ groupBlocks g xs
fromFile _ _ = error "File is too short"


groupBlocks :: StdGen -> [Int] -> [[Int]]
groupBlocks _ [] = []
groupBlocks g (len:xs) = block : groupBlocks g' zs
  where
    (ys, zs) = splitAt (len*4) xs
    shuffledBlock = listArray ((0, 0), (3, len-1)) ys
    (g', block) = elems <$> deshuffleBlock g shuffledBlock


buildImage :: Int -> Int -> [[Int]] -> Image
buildImage h w blks' = img
  where
    blks = listArray (0, length blks' - 1) blks'

    img = Image h w pxls
    pxls = restorePixels (grads, preds) blks
    (grads, preds) = calcGradGAP img


-- Testing purposes only
fucktrace :: Show b => (a -> b) -> a -> a
fucktrace = fucktrace' . (show .)

fucktrace' :: (a -> String) -> a -> a
fucktrace' f x = trace (f x) x

average, mse, mav :: Integral a => [a] -> a
average xs = sum xs `div` fromIntegral (length xs)
mse = average . map (^ (2 :: Int))
mav = average . map abs

type Histogram = Array Int Int
histogram :: [Int] -> Histogram
histogram xs = accumArray (+) 0 (minimum xs, maximum xs) [(x, 1) | x <- xs]

printHistogram :: Histogram -> IO ()
printHistogram h = forM_ (assocs h) $ \(x, y) ->
  when (y > 0) $ printf "%4d -> %4d\n" x y


main :: IO ()
main = do
  lena <- readP5 "lena512.pgm"

  when False $ do
    putStrLn "Remapped Errors:"
    printHistogram . histogram . elems $
      remappedErrors lena (snd $ calcGradGAP lena)
    putStrLn ""

  let
    (grads, preds) = calcGradGAP lena
    ees = calcErrorEnergy grads (predictionErrors lena preds)

  when False $ do
    putStrLn "Error Energy Estimations:"
    printHistogram . histogram . elems $ ees
    putStrLn ""

  let
    g = mkStdGen 100000009
    err's = remappedErrors lena preds
    blocks = partitionErrors (quantizer defaultBins) err's ees
    blocks' = snd $ shuffleBlocks g blocks

    format = intercalate ", " . map (printf "%3d")
    size' blk =
      let (x, y) = snd (bounds blk)
      in (x+1, y+1)

  when False $ forM_ blocks' $ \blk -> do
    printf "Size: %s\n" $ show (size' blk)
    putStrLn . format . take 8 $ elems blk

  writeP5 "lena_out.pgm" . fromFile g . toFile g $ lena
