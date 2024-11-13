{-# OPTIONS_GHC -Wno-type-defaults #-}

module Image (addSmallImageVersions) where

import Codec.Picture (DynamicImage (..), convertRGB8, readImage, savePngImage)
import Codec.Picture.Types (Image (..), PixelRGB8, generateImage, pixelAt)
import Control.Monad (forM_)
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath (splitExtension, (</>))

scaleImage :: Int -> Image PixelRGB8 -> Image PixelRGB8
scaleImage targetHeight (Image width height dat) = generateImage pixelRenderer newWidth targetHeight
  where
    aspectRatio = fromIntegral width / fromIntegral height
    newWidth = floor (aspectRatio * fromIntegral targetHeight)
    pixelRenderer x y =
      pixelAt
        (Image width height dat)
        (floor (fromIntegral x * fromIntegral width / fromIntegral newWidth))
        (floor (fromIntegral y * fromIntegral height / fromIntegral targetHeight))

scaleToHeight :: Int -> DynamicImage -> DynamicImage
scaleToHeight targetHeight img = ImageRGB8 (scaleImage targetHeight $ convertRGB8 img)

processImage :: FilePath -> FilePath -> IO ()
processImage inputPath outputPath =
  readImage inputPath >>= \case
    Left err -> putStrLn $ "Error loading image " ++ inputPath ++ ": " ++ err
    Right img -> do
      let targetHeight = 400
      let scaledImg = scaleToHeight targetHeight img
      savePngImage outputPath scaledImg

getImageFiles :: FilePath -> IO [FilePath]
getImageFiles dir = do
  files <- listDirectory dir
  return
    [ dir </> file
      | file <- files,
        any (`isSuffixOf` file) [".jpg", ".jpeg", ".png", ".bmp", ".gif"]
    ]

addSmallImageVersions :: FilePath -> IO ()
addSmallImageVersions path = do
  let outputSuffix = "-small"
  imageFiles <- getImageFiles path
  forM_ imageFiles $ \inputPath -> do
    let (baseName, ext) = splitExtension inputPath
    let outputPath = baseName ++ outputSuffix ++ ext
    processImage inputPath outputPath
