{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UnicodeSyntax         #-}
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
--, richestDoge
       ) where

import           CoinExchange.Entities       as Entities
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Data.Maybe
import qualified Data.Text                   as T
import           Database.Esqueleto
import qualified Database.Persist.Postgresql as PG

data ValidationError = EmptyNameError | SneakyCatError deriving (Show)

newtype Name = Name T.Text

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

richDoges ∷ MinCoins → Query [Entity DogeName]
richDoges threshold =
    select $
    from $ \(dogeName `InnerJoin` wallet) → do
    on (wallet ^. WalletDogeId ==. dogeName ^. DogeNameDogeId)
    where_ (wallet ^. WalletCoins >=.  val threshold)
    pure dogeName

-- richestDoge ∷ Query [Entity DogeName]
-- richestDoge =
--         select $ from $ \dogeName → do

--         (walletDogeId, totalAssets) ←
--             from $ \wallet → do
--             groupBy (wallet ^. WalletDogeId)
--             pure (wallet ^. WalletDogeId, sum_ (wallet ^. WalletCoins))

--         where_ (dogeName ^. DogeNameDogeId ==. walletDogeId)
--         orderBy [desc totalAssets]
--         pure dogeName
