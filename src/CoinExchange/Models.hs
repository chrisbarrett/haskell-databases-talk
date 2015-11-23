{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}
module CoinExchange.Models
       (
         Entities.migrateAll
       , MonadDB
       , Doge
       , ValidationError(..)
       , Name
       , name
       , registerDoge
       , richDoges
       , dogeAssets
       ) where

import           CoinExchange.Entities       as Entities
import qualified Control.Lens                as Lens
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Data.Maybe                  as Maybe
import qualified Data.Ratio                  as Ratio
import qualified Data.Text                   as T
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as PG


newtype Name = Name T.Text deriving (Show)

data ValidationError = EmptyNameError | SneakyCatError deriving (Show)

name ∷ T.Text → Either ValidationError Name
name = validate . T.strip
  where
    validate ""    = Left EmptyNameError
    validate "cat" = Left SneakyCatError
    validate n     = Right (Name n)



type MonadDB a = ∀ m. MonadIO m ⇒ ReaderT PG.SqlBackend m a
type Query a = ∀ m. MonadIO m ⇒ SqlPersistT m a

registerDoge ∷ Name → MonadDB (Key Doge, Key DogeName)
registerDoge (Name nm) = do
    doge ← PG.insert Doge
    dogeName ← PG.insert $ DogeName doge nm
    pure (doge, dogeName)



type MinCoins = Int

richDoges ∷ MinCoins → Query [Name]
richDoges threshold = do
    names ←
        select $ distinct $
        from $ \(dogeName `InnerJoin` wallet) → do
        on (wallet ^. WalletDogeId ==. dogeName ^. DogeNameDogeId)
        where_ (wallet ^. WalletCoins >=.  val threshold)
        pure dogeName

    pure $ map (Name . dogeNameDogeName . entityVal) names


newtype TotalCoins = TotalCoins Integer deriving (Show)

newtype Id = Id Integer deriving (Show)

dogeAssets ∷ MonadIO m ⇒ ReaderT SqlBackend m [(Id, TotalCoins)]
dogeAssets = do
    assets ←
        select $ from $ \wallet → do
        groupBy (wallet ^. WalletDogeId)
        let total = sum_ (wallet ^. WalletCoins)
        pure (wallet ^. WalletDogeId, total)

    pure $ map fromSql assets
  where
    fromSql = Lens.bimap toId sumToCoins
    toId = Id . toInteger . unSqlBackendKey . unDogeKey . unValue
    sumToCoins = TotalCoins . Ratio.numerator . Maybe.fromMaybe 0 . unValue
