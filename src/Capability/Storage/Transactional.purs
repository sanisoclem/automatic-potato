module AP.Capability.Storage.Transactional where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Argonaut (Json)
import Data.Array (elem, find, (:))
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import AP.Capability.Storage.Cf (class MonadCfStorage, class MonadCfStorageBatch, runBatch, tryGetDurableState)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)
import Safe.Coerce (coerce)

type PutOperation = { docId :: String, body :: Json }

newtype BatchOperation = BatchOperation { puts:: Array PutOperation, deletes :: Array String }

newtype TransactionalStorageT m a = TransactionalStorageT (StateT BatchOperation m a)
derive newtype instance Functor m => Functor (TransactionalStorageT m)
derive newtype instance Monad m =>  Apply (TransactionalStorageT m)
derive newtype instance Monad m => Applicative (TransactionalStorageT m)
derive newtype instance Monad m => Bind (TransactionalStorageT m)
derive newtype instance Monad m => Monad (TransactionalStorageT m)
derive newtype instance MonadThrow e m => MonadThrow e (TransactionalStorageT m)
derive newtype instance Monad m => MonadState BatchOperation (TransactionalStorageT m)
derive newtype instance MonadTrans TransactionalStorageT
derive newtype instance (MonadEffect m) => MonadEffect (TransactionalStorageT m)
derive newtype instance (MonadAff m) => MonadAff (TransactionalStorageT m)

batchOperation :: ∀ m a. MonadCfStorageBatch m => TransactionalStorageT m a -> m a
batchOperation m = do
  (Tuple a batch) <- runStateT (coerce m) (BatchOperation { puts: [], deletes: []})
  runBatch $ coerce batch
  pure a

-- TODO: error when delete + put in the same batch
batchputDurableState' :: ∀ m. Monad m => String -> Json -> TransactionalStorageT m Unit
batchputDurableState' id doc = modify_ <<< setter $ (:) { docId: id, body: doc }
  where
    setter fn (BatchOperation x) = BatchOperation x { puts = fn x.puts }

batchdeleteDurableState' :: ∀ m. Monad m => String -> TransactionalStorageT m Unit
batchdeleteDurableState' id = modify_ <<< setter $ (:) id
  where
    setter fn (BatchOperation x) = BatchOperation x { deletes = fn x.deletes }

batchtryGetDurableState' :: ∀ m. MonadCfStorage m => String -> TransactionalStorageT m (Maybe Json)
batchtryGetDurableState' key = do
  (BatchOperation batch) <- get
  let isDeleted = elem key batch.deletes
  let lastUpdate = find ((==) key <<< _.docId) batch.puts
  case { isDeleted, lastUpdate } of
    { isDeleted: true, lastUpdate: _ } -> pure Nothing
    { isDeleted:_, lastUpdate: Just x } -> pure $ Just x.body
    _ -> lift $ tryGetDurableState key

class Monad m <= MonadTransactionalStorage m where
  batchtryGetDurableState :: String -> m (Maybe Json)
  batchputDurableState :: String -> Json -> m Unit
  batchdeleteDurableState :: String -> m Unit

instance monadTransactionalStorageTransactionalStorageT :: (Monad m, MonadCfStorage m) => MonadTransactionalStorage (TransactionalStorageT m) where
  batchtryGetDurableState = batchtryGetDurableState'
  batchputDurableState = batchputDurableState'
  batchdeleteDurableState = batchdeleteDurableState'

batchGetDurableState :: ∀ m. (MonadThrow Error m) => (MonadTransactionalStorage m) => String -> m Json
batchGetDurableState key = batchtryGetDurableState key >>= note (error $ "state not found: " <> key) >>> liftEither
