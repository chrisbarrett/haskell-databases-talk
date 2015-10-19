{-# LANGUAGE OverloadedStrings #-}
module Main where

import           CoinExchange.Entities       (dogeNameDogeName)
import qualified CoinExchange.Models         as Model
import           Control.Monad.Logger        as Logger
import qualified Data.Text.IO                as T
import qualified Database.Persist.Postgresql as DB
import qualified Views.Table                 as View


connStr :: DB.ConnectionString
connStr = "host=localhost dbname=dogecoin"

connTimeout :: Int
connTimeout = 10


main :: IO ()
main = do
    runDB $ DB.runMigration Model.migrateAll
    doges <- runDB $ Model.richDoges 200
    let dogeNames = map (dogeNameDogeName . DB.entityVal) doges
        table = View.render $ View.makeTable "Richest Doges" dogeNames
    T.putStrLn table


runDB :: DB.SqlPersistM a -> IO a
runDB f =
    Logger.runNoLoggingT $
    DB.withPostgresqlPool connStr connTimeout $
    NoLoggingT . DB.runSqlPersistMPool f
