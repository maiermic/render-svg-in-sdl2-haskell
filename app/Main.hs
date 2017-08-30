module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.Vector.Storable.Mutable (IOVector)
import Graphics.Rasterific.Svg
       (loadCreateFontCache, renderSvgDocument)
import Graphics.Svg (loadSvgFile)

import Data.Function (fix)
import Data.Word (Word8)

import Control.Monad as CM
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

import Foreign.C.Types (CInt)
import SDL.Vect (V2(V2))
import qualified SDL

import Paths_svg_loading (getDataFileName)

-- SDL dependency: sudo apt-get install libsdl2-dev
main :: IO ()
main = do
  mimage <- getDataFileName "thumbs-up.svg" >>= loadSVGImage
  case mimage of
    Nothing -> putStrLn "Image convertion failed."
    (Just image) -> do
      let surfaceSize :: V2 CInt
          surfaceSize = V2 1280 720
      surface <- createSurfaceFromSVG image surfaceSize
            -- show the surface
      putStrLn "Hopefully showing the image."

createSurfaceFromSVG :: Image PixelRGBA8 -> V2 CInt -> IO SDL.Surface
createSurfaceFromSVG image surfaceSize = do
  let rawImageData :: Vector Word8
      rawImageData = imageData image
      imWidth :: Int
      imWidth = imageWidth image
      pitch :: CInt
      pitch = fromIntegral imWidth
  mutableVector <- convertToMutableVector rawImageData
  SDL.createRGBSurfaceFrom mutableVector surfaceSize pitch SDL.RGBA8888

-- | as per https://github.com/haskell/vector/issues/175, there are some missing convertion functions
--   so we have to create one by hand
convertToMutableVector :: Vector Word8 -> IO (IOVector Word8)
convertToMutableVector v = do
  let len = V.length v
  mv <- GM.new len
  CM.forM_ [0 .. (len - 1)] $ \i -> GM.write mv i (v V.! i)
  return mv

loadSVGImage :: FilePath -> IO (Maybe (Image PixelRGBA8))
loadSVGImage filepath = do
  mdoc <-  loadSvgFile filepath
  case mdoc of
    Nothing -> return Nothing
    Just doc -> do
      cache <- loadCreateFontCache "fonty-texture-cache"
      (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
      return $ Just finalImage