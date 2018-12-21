{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Import & Apply a Price & Status Update CSV. -}

import qualified Data.ByteString.Lazy as LBS
import Data.Csv ((.:), FromNamedRecord(..), FromField(..), decodeByName)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Database.Persist.Sql ((=.), (==.), Entity(..), Update, toSqlKey, update, selectFirst)
import Numeric (readSigned, readFloat)

import DB

main :: IO ()
main = fromCsvFile "sese-price-status-update.csv" >>= updateProducts . filterUpdates

data ImportData
    = ImportData
        { productId :: Integer
        , price :: Maybe MyPrice
        , organic :: Maybe MyBool
        , eco :: Maybe MyBool
        }
    deriving (Show)

newtype MyBool = MyBool Bool deriving (Show)
newtype MyPrice = MyPrice Rational deriving (Show)

instance FromNamedRecord ImportData where
    parseNamedRecord m =
        ImportData
            <$> m .: "productId"
            <*> m .: "2019 price"
            <*> m .: "2019 organic"
            <*> m .: "2019 grower"

instance FromField MyBool where
    parseField s = MyBool . (== (1 :: Integer)) <$> parseField s

instance FromField MyPrice where
    parseField s = MyPrice . fst . head . readSigned readFloat <$> parseField s

fromCsvFile :: Text -> IO [ImportData]
fromCsvFile fileName = do
    rawText <- LBS.readFile $ T.unpack fileName
    case decodeByName rawText of
        Left err ->
            error err
        Right (_, vec) ->
            return $ V.toList vec

filterUpdates :: [ImportData] -> [(ProductId, [Update Product], [Update ProductIcon])]
filterUpdates = mapMaybe buildUpdates
  where
    buildUpdates :: ImportData -> Maybe (ProductId, [Update Product], [Update ProductIcon])
    buildUpdates d =
        case (price d, organic d, eco d) of
            (Nothing, Nothing, Nothing) -> Nothing
            (p, o, e) -> Just
                ( toSqlKey $ fromIntegral $ productId d
                , maybe [] makePrice p
                , concat $ catMaybes
                    [ fmap makeOrganic o
                    , fmap makeEco e
                    ]
                )
    makePrice (MyPrice p) =
        [ProductPrice =. p]
    makeOrganic (MyBool b) =
        [ProductIconIsOrganic =. b]
    makeEco (MyBool b) =
        [ProductIconIsEco =. b]

updateProducts :: [(ProductId, [Update Product], [Update ProductIcon])] -> IO ()
updateProducts = runDB . mapM_ updateProduct
  where
    updateProduct :: (ProductId, [Update Product], [Update ProductIcon]) -> DB ()
    updateProduct (pId, productUpdates, iconUpdates) = do
        update pId productUpdates
        selectFirst [ProductIconProduct ==. pId] [] >>= \case
            Just (Entity iconId _) ->
                update iconId iconUpdates
            Nothing ->
                error "handle the not found case!"
