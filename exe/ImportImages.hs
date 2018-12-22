{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Resize & Optimize Product Images or Update the Image Paths in the DB. -}
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Csv ((.:), FromNamedRecord(..), decodeByName)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Database.Persist ((==.), (=.), updateWhere)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, copyFile)
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName, takeExtension)
import System.Process.Typed (runProcess_, proc)

import DB (DB, runDB)
import Schema

main :: IO ()
main =
    getArgs >>= \case
        ["thumbnails"] ->
            makeThumbnails
        _ ->
            updateProductImages

makeThumbnails :: IO ()
makeThumbnails = do
    originalImages <- filter (`notElem` [".", ".."]) <$> getDirectoryContents "raw"
    createDirectoryIfMissing True "output/large"
    createDirectoryIfMissing True "output/medium"
    mapM_ processImage originalImages

-- | Thumbnail, Categorize, & Optimize an Image
processImage :: FilePath -> IO ()
processImage imageName = do
    let relativePath = "raw/" <> imageName
        baseName = takeBaseName imageName
        ext = takeExtension imageName

        smallName = imageName
        smallPath = "output/" <> smallName

        mediumName = baseName <> "_MED" <> ext
        mediumPath = "output/medium/" <> mediumName

        largeName = baseName <> "_LRG" <> ext
        largePath = "output/large/" <> largeName
    copyFile relativePath largePath
    resize relativePath mediumPath 282
    resize relativePath smallPath 100
    if ext == ".png" then
        runProcess_ $ proc "optipng"
            [ "-o7"
            , "-strip"
            , "all"
            , smallPath
            , mediumPath
            , largePath
            ]
    else if ext == ".jpg" then
        runProcess_ $ proc "jpegoptim"
            [ "-s"
            , "--all-progressive"
            , smallPath
            , mediumPath
            , largePath
            ]
    else
        error $ "unexpect ext in processImage: " <> ext

resize :: FilePath -> FilePath -> Int -> IO ()
resize source dest width =
    runProcess_ $ proc "convert" [ source, "-resize", show width <> ">", dest]



updateProductImages :: IO ()
updateProductImages =
    fromCsvFile "sese-new-images.csv"
    >>= runDB . mapM_ updateProductImage

updateProductImage :: ImportData -> DB ()
updateProductImage d =
    unless (sku d == "" || imageFilename d == "")
        $ updateWhere [ProductModel ==. sku d]
            [ProductImage =. Just (imageFilename d)]

data ImportData
    = ImportData
        { sku :: Text
        , imageFilename :: Text
        } deriving (Show)

instance FromNamedRecord ImportData where
    parseNamedRecord m =
        ImportData
            <$> m .: "SKU"
            <*> m .: "Image"

fromCsvFile :: Text -> IO [ImportData]
fromCsvFile fileName = do
    rawText <- LBS.readFile $ T.unpack fileName
    case decodeByName rawText of
        Left err -> error err
        Right (_, vec) ->
            return $ V.toList vec
