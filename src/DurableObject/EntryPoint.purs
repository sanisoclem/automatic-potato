module AP.DurableObject.EntryPoint where

import Prelude

import AP.Capability.Fetch (fromRequest, getRequestMethod)
import AP.Capability.Storage.Transactional (batchOperation)
import AP.Data.Fetch (RequestMethod(..), Response, errorResponse, jsonResponse, messageResponse, notFoundResponse)
import AP.DurableObject.DurableObjectM (DurableObjectM, runDurableObjectM, mkContext)
import AP.DurableObject.Ledger (handleCommand, handleQuery)
import AP.Foreign.DurableObject (DurableObjectRequest, DurableObjectResponse, DurableObjectState, doStringResponse)
import Control.Monad.Error.Class (catchError)
import Control.Promise (Promise, fromAff)
import Data.Argonaut (stringify)
import Effect (Effect)

toDurableObjectResponse :: Response -> DurableObjectResponse
toDurableObjectResponse x = doStringResponse (stringify x.body) x.statusCode

toResponse :: DurableObjectM Response -> DurableObjectM DurableObjectResponse
toResponse x = toDurableObjectResponse <$> catchError x (pure <<< errorResponse)

ledgerFetchMain :: DurableObjectState -> DurableObjectRequest -> Effect (Promise DurableObjectResponse)
ledgerFetchMain state req = fromAff $ runDurableObjectM (mkContext state req) $ toResponse do
  getRequestMethod >>= case _ of
    PUT -> do
      cmd <- fromRequest
      batchOperation $ handleCommand cmd
      pure $ messageResponse 200 "OK"
      -- TODO; schedule an alarm
    GET -> do
      qry <- fromRequest
      result <- handleQuery qry
      pure $ jsonResponse 200 result
    _ -> pure $ notFoundResponse "not found"