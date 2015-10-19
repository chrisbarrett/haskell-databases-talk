{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
module CoinExchange.Models
       (
         MonadDB
       , Doge
       , ValidationError(..)
       , Name
       , name
       , registerDoge
       , nonEmptyWallets
       , addCoin
       , richDoges
       , richestDoge
       ) where

import           CoinExchange.Entities
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Data.Maybe
import qualified Data.Text                   as T
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as PG

data ValidationError = EmptyNameError | SneakyCatError deriving (Show)

newtype Name = Name T.Text

name :: T.Text -> Either ValidationError Name
name = validate . T.strip
  where
    validate ""    = Left EmptyNameError
    validate "cat" = Left SneakyCatError
    validate n     = Right (Name n)

type MonadDB = ReaderT PG.SqlBackend

registerDoge :: MonadIO m => Name -> MonadDB m (Key Doge, Key DogeName)
registerDoge (Name nm) = do
    doge <- PG.insert Doge
    dogeName <- PG.insert $ DogeName doge nm
    pure (doge, dogeName)

addCoin :: MonadIO m => Key Wallet -> MonadDB m (Maybe (Entity Wallet))
addCoin k = undefined

type MinCoins = Int

richDoges :: MonadIO m => MinCoins -> MonadDB m [Entity DogeName]
richDoges threshold =
    select $
    from $ \(dogeName `InnerJoin` wallet) -> do
    on (wallet ^. WalletDogeId ==. dogeName ^. DogeNameDogeId)
    where_ (wallet ^. WalletCoins >=. val threshold)
    pure dogeName

nonEmptyWallets :: MonadIO m => MonadDB m [(Value (Key Doge))]
nonEmptyWallets =
  select $
  from $ \wallet -> do
  where_ (wallet ^. WalletCoins >. val 0)
  pure (wallet ^. WalletDogeId)

richestDoge :: MonadIO m => MonadDB m (Maybe (Entity DogeName))
richestDoge =
    fmap listToMaybe (
        select $
        from $ \(dogeName `InnerJoin` wallet) -> do
        on (wallet ^. WalletDogeId ==. dogeName ^. DogeNameDogeId)
        orderBy [desc (wallet ^. WalletCoins)]
        limit 1
        pure dogeName
    )
