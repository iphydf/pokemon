module Pokemon.Profile where

import           Data.Default.Class (Default (..))


data Profile = Profile
  { username :: String
  , password :: String
  , address  :: String
  }
  deriving (Eq, Show, Read)


instance Default Profile where
  def = Profile
    "<your Pokemon Trainer Club username>"
    "<your password>"
    "<your player location>"


-- vim:sw=2
