{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module CoinExchange.Entities where

import           Data.Text           (Text)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Doge

DogeName
  dogeId DogeId
  dogeName Text
  UniqueDogeName dogeId dogeName
  deriving Eq Show

Wallet
  dogeId DogeId
  coins Int
  deriving Eq Show

Pack
  packName Text
  deriving Eq Show

PackMember
  packId PackId
  dogeId DogeId
  UniquePackMember packId dogeId
  deriving Eq Show

|]
