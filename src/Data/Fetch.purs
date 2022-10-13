module AP.Data.Fetch where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut.Core as A
import Data.Tuple (Tuple(..))
import Effect.Aff (Error)
import Foreign.Object (fromFoldable)


data RequestMethod
  = GET
  | POST
  | DELETE
  | PUT
  | Unknown String

type Response =
  { statusCode :: Int
  , body :: Json
  }

jsonResponse :: Int -> Json -> Response
jsonResponse = { statusCode: _, body: _ }

messageResponse :: Int -> String -> Response
messageResponse code msg = { statusCode: _, body: _ } code $ json
  where
    json = A.fromObject ( fromFoldable [ Tuple "message" (A.fromString msg) ] )

errorResponse :: Error -> Response
errorResponse err = messageResponse 500 $ show err

notFoundResponse :: String -> Response
notFoundResponse msg = messageResponse 404 $ msg