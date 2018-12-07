module Export
    ( toCsvFile
    )
where

import           Data.Csv                       ( ToNamedRecord
                                                , DefaultOrdered
                                                , encodeDefaultOrderedByName
                                                )

import qualified Data.ByteString.Lazy          as LBS

toCsvFile :: (DefaultOrdered a, ToNamedRecord a) => String -> [a] -> IO ()
toCsvFile fileName recordList =
    LBS.writeFile fileName $ encodeDefaultOrderedByName recordList
