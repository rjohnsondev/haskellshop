-- Based on http://hackage.haskell.org/package/thumbnail

module ShopData.Thumbnail
       ( ImageFormat(..)
       , Thumbnail(..)
       , mkThumbnail
       , mkThumbnail'
       , defaultBounds) where

import Prelude
import Graphics.GD
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L

data ImageFormat = Gif | Jpeg | Png

data Thumbnail = Thumbnail { fmt :: ImageFormat     -- ^ Image Format Type
                           , img :: Image           -- ^ Thumbnail Image
                           , sz  :: Size            -- ^ Thumbnail Size
                           , lbs :: L.ByteString    -- ^ Thumbnail Data
                           , orgImg :: Image        -- ^ Original Image
                           , orgSZ :: Size          -- ^ Original Size
                           , saveFile :: FilePath -> IO ()
                           }

-- | Create a thumbnails with the default size
mkThumbnail :: L.ByteString -> IO (Either String Thumbnail)
mkThumbnail = mkThumbnail' defaultBounds

-- | Create a thumbnail from a specific subregion of the image
mkThumbnail' :: ((Int,Int),(Int,Int)) -> L.ByteString -> IO (Either String Thumbnail)
mkThumbnail' sizeBounds = thumbnail . L.unpack
  where
    thumbnail ws | length ws >= 3 = thumbnail' ws -- FIXME!
                 | otherwise = return $ Left "unsupported image format"
    
    thumbnail' ws@(0xff:0xd8:_) = thumbnailJpeg ws
    thumbnail' ws@(0x89:0x50:_) = thumbnailPng ws
    thumbnail' ws@(0x47:0x49:0x46:_) = thumbnailGif ws
    thumbnail' _ = return $ Left "unsupported image format"
    
    thumbnailJpeg ws = do
      src <- loadJpegByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize sizeBounds size
      thm <- uncurry resizeImage size' dest
      bs <- saveJpegByteString 90 thm
      let save fp = saveJpegFile 90 fp thm
      return $ Right Thumbnail { fmt=Jpeg
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               , saveFile=save
                               }
    
    thumbnailPng ws = do
      src <- loadPngByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize sizeBounds size
      thm <- uncurry resizeImage size' dest
      bs <- savePngByteString thm
      let save fp = savePngFile fp thm
      return $ Right Thumbnail { fmt=Png
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               , saveFile=save
                               }
      
    thumbnailGif ws = do
      src <- loadGifByteString $ BS.pack ws
      size <- imageSize src
      dest <- copyImage src
      let size' = newSize sizeBounds size
      thm <- uncurry resizeImage size' dest
      bs <- saveGifByteString thm
      let save fp = saveGifFile fp thm
      return $ Right Thumbnail { fmt=Gif
                               , img=thm
                               , sz=size'
                               , lbs=strictToLazy bs
                               , orgImg=src
                               , orgSZ=size
                               , saveFile=save
                               }
        
    strictToLazy = L.pack . BS.unpack
    
newSize :: ((Int,Int),(Int,Int)) -> Size -> Size
newSize ((wMin,hMin),(wMax,hMax)) (w, h) | w >= h && wMax*h`div`w > wMin = (wMax, wMax*h`div`w)
               | w >= h && h >= hMin           = (hMin*w`div`h, hMin)
               | w <  h && hMax*w`div`h > hMin = (hMax*w`div`h, hMax)
               | w <  h && w >= wMin           = (wMin, wMin*h`div`w)
               | otherwise = (w, h)

defaultBounds :: ((Int,Int),(Int,Int))
defaultBounds = ((20,20),(60,60))
