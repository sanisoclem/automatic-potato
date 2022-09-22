module AP.Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)

class Monad m <= MonadNavigate r m where
  navigate :: r -> m Unit

class Monad m <= MonadNavigateAbs m where
  navigateAbs :: String -> m Unit