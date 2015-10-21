{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import qualified CoinExchange.Models         as Model
import qualified Control.Monad.Logger        as Logger
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Database.Persist.Postgresql as DB
import qualified Views.Table                 as View

connStr = "host=localhost dbname=dogecoin"

connTimeout = 10

richnessThreshold = 200

main ∷ IO ()
main = do
    runMigrations
    printRichList
    printBlankLine
    printAssets

  where

    runMigrations = runDB $ DB.runMigration Model.migrateAll

    printBlankLine = T.putStrLn "\n"

    printRichList = do
        doges ← runDB $ Model.richDoges richnessThreshold
        let rows = map (T.pack . show) doges
        T.putStrLn $ View.render $ View.makeTable "Richest Doges" rows

    printAssets = do
        assets ← runDB Model.dogeAssets
        let rows = map View.renderTuple assets
        T.putStrLn $ View.render $ View.makeTable "Balances" rows


runDB ∷ DB.SqlPersistM a → IO a
runDB f =
    Logger.runNoLoggingT $
    DB.withPostgresqlPool connStr connTimeout $
    Logger.NoLoggingT . DB.runSqlPersistMPool f
