module AP.DurableObject.DurableObjectM where

import Prelude

import AP.Capability.Fetch (class MonadFetchRequest)
import AP.Capability.Has (class HasLens, getState)
import AP.Capability.Storage.Cf (class MonadCfStorage, class MonadCfStorageBatch)
import AP.Data.Fetch (RequestMethod(..))
import AP.Foreign.DurableObject (DurableObjectRequest, DurableObjectState, doBatchState, dodeleteDurableState, dogetDurableState, dogetDurableStateByPrefix, doputDurableState, doRequestGetBody, doRequestGetMethod, doRequestGetParam, doRequestGetPath, mkBatchedPut)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Array (fromFoldable)
import Data.Lens (Iso', iso)
import Data.Lens.Record (prop)
import Data.Tuple (fst)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))

newtype DurableObjectM a = DurableObjectM (StateT ContextData Aff a)

runDurableObjectM :: ∀ a. ContextData -> DurableObjectM a -> Aff a
runDurableObjectM state = map fst <<< (runStateT <@> state) <<< coerce

derive newtype instance Functor DurableObjectM
derive newtype instance Apply DurableObjectM
derive newtype instance Applicative DurableObjectM
derive newtype instance Bind DurableObjectM
derive newtype instance Monad DurableObjectM
derive newtype instance MonadThrow Error DurableObjectM
derive newtype instance MonadError Error DurableObjectM
derive newtype instance MonadEffect DurableObjectM
derive newtype instance MonadAff DurableObjectM
derive newtype instance MonadState ContextData DurableObjectM

instance MonadCfStorage DurableObjectM where
  tryGetDurableState key = do
    state <- getState
    liftAff $ dogetDurableState state key
  putDurableState key value = do
    state <- getState
    liftAff <<< doputDurableState state key $ value
  deleteDurableState key = liftAff <<< (flip dodeleteDurableState) key =<< getState
  getDurableStateByPrefix prefix = do
    state <- getState
    resultMap <- liftAff <<< dogetDurableStateByPrefix state $ prefix
    pure $ fromFoldable resultMap

instance MonadCfStorageBatch DurableObjectM where
  runBatch batch = do
    let puts = (mkBatchedPut <$> _.docId <*> _.body) <$> batch.puts
    state <- getState
    liftAff $ doBatchState state puts batch.deletes

instance MonadFetchRequest DurableObjectM where
  getRequestMethod = do
    request <- getState
    pure $ case doRequestGetMethod request of
      "POST" -> POST
      "GET" -> GET
      "DELETE" -> DELETE
      "PUT" -> PUT
      x -> Unknown x
  getBodyString = do
    request <- getState
    liftAff <<< doRequestGetBody $ request
  getPath = do
    req <- getState
    pure $ doRequestGetPath req
  tryGetParam key = do
    req <- getState
    pure $ doRequestGetParam req key

data ContextData = ContextData
  { durableObjectRequest :: DurableObjectRequest
  , durableObjectState :: DurableObjectState
  }

mkContext :: DurableObjectState -> DurableObjectRequest -> ContextData
mkContext durableObjectState durableObjectRequest =
  ContextData
    { durableObjectRequest
    , durableObjectState
    }

_ContextData :: Iso' ContextData { durableObjectRequest :: DurableObjectRequest, durableObjectState :: DurableObjectState }
_ContextData = iso (case _ of ContextData x -> x) ContextData

instance hasContextState :: HasLens ContextData DurableObjectState  where
  focus = _ContextData <<< prop (Proxy :: Proxy "durableObjectState")

instance hasContextRequest :: HasLens ContextData DurableObjectRequest where
  focus = _ContextData <<<  prop (Proxy :: Proxy "durableObjectRequest")
