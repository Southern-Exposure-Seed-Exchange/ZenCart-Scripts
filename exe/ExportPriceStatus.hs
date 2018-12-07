{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Ratio                     ( numerator
                                                , denominator
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
        >>= toCsvFile "price-and-status-export.csv"
        .   map toExport

toExport
    :: ( Entity Product
       , Maybe (Entity ProductIcon)
       , Maybe (Entity ProductDescription)
       )
    -> ExportData
toExport (Entity pId product, maybeIcons, maybeDescription)
    = let
          productId         = fromIntegral $ fromSqlKey pId
          sku               = productModel product
          name = maybe "" (productDescriptionName . entityVal) maybeDescription
          price             = T.pack . displayRational 2 $ productPrice product
          (organic, grower) = mapBoth fromBool $ case maybeIcons of
              Nothing -> (False, False)
              Just (Entity _ icons) ->
                  (productIconIsOrganic icons, productIconIsEco icons)
      in
          ExportData {..}
  where
    fromBool True  = "1"
    fromBool False = "0"
    mapBoth f (a, b) = (f a, f b)


data ExportData
    = ExportData
        { productId :: Int
        , sku :: Text
        , name :: Text
        , price :: Text
        , organic :: Text
        , grower :: Text
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

displayRational :: Int -> Rational -> String
displayRational len rat = sign ++ shows d ("." ++ paddedDecimal)
  where
    sign          = if num < 0 then "-" else ""
    (d, next)     = abs num `quotRem` den
    num           = numerator rat
    den           = denominator rat
    decimalPart   = take len (go next)
    paddedDecimal = decimalPart ++ replicate (len - length decimalPart) '0'

    go 0 = ""
    go x = let (d', n) = (10 * x) `quotRem` den in shows d' (go n)
