module AP.UI.AppM where

import Prelude

import AP.Capability.Log (class MonadLog, logDebug)
import AP.Data.Log as Log
import AP.Data.Utility (convertJsonErrorToError)
import AP.Domain.Ledger.Command (LedgerCommand(..))
import AP.UI.Capability.ApiClient (class MonadApiClient, Ledger)
import AP.UI.Capability.Navigate (class MonadNavigate, class MonadNavigateAbs)
import AP.UI.Route (Route)
import AP.UI.Route as Route
import AP.UI.Store (Action(..), EnvironmentType(..), Store)
import AP.UI.Store as Store
import Affjax.RequestBody (json)
import Affjax.ResponseFormat as AXRF
import Affjax.Web (printError)
import Affjax.Web as AX
import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftEither)
import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, error)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Routing.Duplex (print)
import Safe.Coerce (coerce)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadStore Action Store AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM

instance MonadLog AppM where
  logMessage log = do
    { envType } <- getStore
    liftEffect case envType, Log.level log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance MonadNavigate Route AppM where
  navigate x = do
    { psi } <- getStore
    liftEffect $ psi.pushState (unsafeToForeign unit) $ print Route.routeCodec x

instance MonadNavigateAbs AppM where
  navigateAbs href = liftEffect $ setHref href =<< location =<< window

liftReq x = (liftEither <<< lmap (error <<< printError)) <=< liftAff $ x

postCommand :: String -> LedgerCommand -> AppM Unit
postCommand ledgerId cmd =
  void <<< liftReq $ AX.put AXRF.json ("/api/ledger/" <> ledgerId) (Just <<< json <<< encodeJson $ cmd)

postQuery :: forall a. DecodeJson a => String -> String -> AppM a
postQuery ledgerId qry = do
  response <- liftReq $ AX.get AXRF.json ("/api/ledger/" <> ledgerId <> "/" <> qry)
  liftEither <<< convertJsonErrorToError <<< decodeJson $ response.body

getLedger :: String -> AppM Ledger
getLedger id = do
  result <- postQuery id "GetLedgerV1"
  pure { ledgerId: id, result }

getLedgers :: AppM (Array Ledger)
getLedgers = do
  response <- liftReq $ AX.get AXRF.json ("/api/ledger")
  ids <- liftEither <<< convertJsonErrorToError <<< decodeJson $ response.body
  sequence $ getLedger <$> ids

refreshLedgerList :: AppM Unit
refreshLedgerList = do
  ledgers <- getLedgers
  updateStore $ UpdateLedgers ledgers
  logDebug "refreshed ledger list"

instance MonadApiClient AppM where
  getSession = do
    response <- liftAff $ AX.get AXRF.json ("/api/session")
    pure $ hush <<< decodeJson <<< _.body =<< hush response
  refreshLedgerList = refreshLedgerList
  createLedger name = do
    response <- liftReq $ AX.post AXRF.json ("/api/ledger") Nothing
    id <- liftEither <<< convertJsonErrorToError <<< decodeJson $ response.body
    postCommand id $ UpdateLedgerV1 { name }
    refreshLedgerList
  createAccount newLedger = do
    postCommand newLedger.ledgerId $ CreateAccountV1
      { name: newLedger.name
      , denomination: newLedger.denomination
      , accountType: newLedger.accountType
      }
    refreshLedgerList
  updateBalances ledgerId = do
    bal <- postQuery ledgerId "GetBalancesV1"
    updateStore $ UpdateBalances ledgerId bal
