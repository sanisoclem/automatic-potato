module AP.UI.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Generic (noArgs, sum)

data Route
  = Home
  | Dashboard

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Dashboard": "dashboard" / noArgs
  }
