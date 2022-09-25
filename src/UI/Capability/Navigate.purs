module AP.UI.Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (class MonadTrans, lift)

class Monad m <= MonadNavigate r m where
  navigate :: r -> m Unit

class Monad m <= MonadNavigateAbs m where
  navigateAbs :: String -> m Unit

instance (MonadTrans t, Monad (t m), MonadNavigate r m) => MonadNavigate r (t m) where
  navigate = lift <<< navigate

instance (MonadTrans t, Monad (t m), MonadNavigateAbs m) => MonadNavigateAbs (t m) where
  navigateAbs = lift <<< navigateAbs