{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-| Import & Apply a Product Description Update CSV. -}

import qualified Data.ByteString.Lazy          as LBS
import           Data.Csv                       ( (.:)
                                                , FromNamedRecord(..)
                                                , decodeByName
                                                )
import           Database.Persist.Sql           ( (=.)
                                                , (==.)
                                                , updateWhere
                                                , toSqlKey
                                                )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V

import           DB

main :: IO ()
main = fromCsvFile "sese-product-descriptions-update.csv" >>= updateProducts

data ImportData
    = ImportData
        { productId :: Integer
        , description :: T.Text
        }
    deriving (Show)

instance FromNamedRecord ImportData where
    parseNamedRecord m = ImportData <$> m .: "productId" <*> m .: "description"


fromCsvFile :: T.Text -> IO [ImportData]
fromCsvFile fileName = do
    rawText <- LBS.readFile $ T.unpack fileName
    case decodeByName rawText of
        Left  err      -> error err
        Right (_, vec) -> return $ V.toList vec

updateProducts :: [ImportData] -> IO ()
updateProducts = runDB . mapM_ updateProduct
  where
    updateProduct :: ImportData -> DB ()
    updateProduct ImportData { productId, description } = updateWhere
        [ProductDescriptionProduct ==. toSqlKey (fromIntegral productId)]
        [ProductDescriptionDescription =. description]
