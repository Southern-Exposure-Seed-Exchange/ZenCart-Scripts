{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DB
    ( module Schema
    , DB
    , runDB
    , getProductData
    )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( NoLoggingT
                                                , runNoLoggingT
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Database.Persist.MySQL
import           System.Environment             ( lookupEnv )

import           Schema

type DB a = ReaderT SqlBackend (NoLoggingT IO) a


runDB :: DB a -> IO a
runDB f = do
    let
        errMsg
            = "You must supply the DB_USER, DB_PASS, & DB_NAME environmental variables."
    (dbUser, dbPassword, dbName) <-
        liftIO
        $   (,,)
        <$> lookupEnv "DB_USER"
        <*> lookupEnv "DB_PASS"
        <*> lookupEnv "DB_NAME"
        >>= \case
                (Just u, Just p, Just n) -> return (u, p, n)
                _                        -> error errMsg
    runNoLoggingT $ withMySQLConn
        defaultConnectInfo { connectUser     = dbUser
                           , connectPassword = dbPassword
                           , connectDatabase = dbName
                           }
        (runSqlConn f)


getProductData
    :: [Filter Product]
    -> DB
           [ ( Entity Product
             , Maybe (Entity ProductIcon)
             , Maybe (Entity ProductDescription)
             )
           ]
getProductData filters = selectList filters [Asc ProductModel]
    >>= mapM getAdditionalData
  where
    getAdditionalData e@(Entity pId _) =
        (e, , )
            <$> selectFirst [ProductIconProduct ==. pId]        []
            <*> selectFirst [ProductDescriptionProduct ==. pId] []
