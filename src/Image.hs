module Image where

import Data.Array.IArray
import Text.Printf
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C

type IntMat = Array (Int, Int) Int

data Image = Image
  { imgHeight :: Int
  , imgWidth :: Int
  , imgPixels :: IntMat
  } deriving (Show, Eq)


safeParseP5 :: C.ByteString -> Maybe Image
safeParseP5 bs = parse' . words . removeComments . C.unpack $ bs
  where
    removeComments = unlines . removeComment . lines
    removeComment = map $ takeWhile (/= '#')

    takeEnd n xs = C.drop (C.length xs - n) xs

    parse' (magic : hstr : wstr : maxValue : _) = do
      guard $ magic == "P5"
      guard $ read maxValue == (255 :: Int)
      let
        [h, w] = map read [hstr, wstr]
        raster = takeEnd (fromIntegral (h*w)) bs
        toArr = listArray ((1, 1), (h, w))
        parseRaster = toArr . map fromEnum . C.unpack
      return $ Image h w (parseRaster raster)
    parse' _ = Nothing

parseP5 :: C.ByteString -> Image
parseP5 = maybe (error "Failed to parse image") id . safeParseP5

readP5 :: FilePath -> IO Image
readP5 filepath = parseP5 <$> C.readFile filepath

-- parseP5 . formatP5 should be id
-- At least it's true for lena
formatP5 :: Image -> C.ByteString
formatP5 (Image height width pxls) =
  C.append header rasterScan
  where
    header = C.pack . unlines $
      [ "P5"
      , printf "%d %d" height width
      , "255"
      , ""]
    rasterScan = C.pack . map toEnum . elems $ pxls

writeP5 :: FilePath -> Image -> IO ()
writeP5 = (. formatP5) . C.writeFile
