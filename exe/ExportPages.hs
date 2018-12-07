{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                )
import           Data.Text                      ( Text )
import           Database.Persist               ( Entity(..)
                                                , SelectOpt(Asc)
                                                , selectList
                                                )
import           Database.Persist.Sql           ( fromSqlKey )
import           GHC.Generics                   ( Generic )

import           DB
import           Export                         ( toCsvFile )

import qualified Data.Text                     as T


main :: IO ()
main =
    runDB (selectList [] [Asc PageTitle])
        >>= toCsvFile "pages-export.csv"
        .   map toExport

toExport :: Entity Page -> ExportData
toExport (Entity pId page) =
    let pageId    = fromIntegral $ fromSqlKey pId
        title     = pageTitle page
        content   = pageContent page
        wordCount = length $ T.words content
    in  ExportData {..}

data ExportData
    = ExportData
        { pageId :: Int
        , title :: Text
        , content :: Text
        , wordCount :: Int
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData
