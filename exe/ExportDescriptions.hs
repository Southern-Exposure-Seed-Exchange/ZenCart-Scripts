{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..)
                                                , (==.)
                                                )
import           Database.Persist.Sql           ( fromSqlKey )
import           GHC.Generics                   ( Generic )

import           DB
import           Export                         ( toCsvFile )

import qualified Data.Text                     as T

import           Prelude                 hiding ( product )


main :: IO ()
main =
    runDB (getProductData [ProductStatus ==. True])
        >>= toCsvFile "descriptions-export.csv"
        .   map toExport

toExport
    :: ( Entity Product
       , Maybe (Entity ProductIcon)
       , Maybe (Entity ProductDescription)
       )
    -> ExportData
toExport (Entity pId product, _, maybeDescription) =
    let productId          = fromIntegral $ fromSqlKey pId
        sku                = productModel product
        name               = fromDescription productDescriptionName
        currentDescription = fromDescription productDescriptionDescription
        wordCount          = length $ T.words currentDescription
    in  ExportData {..}
    where fromDescription f = maybe "" (f . entityVal) maybeDescription

data ExportData
    = ExportData
        { productId :: Int
        , sku :: Text
        , name :: Text
        , currentDescription :: Text
        , wordCount :: Int
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData
