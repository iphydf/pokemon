{-# LANGUAGE DeriveGeneric #-}
module Pokemon.LoginToken where

import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics         (Generic)


data LoginToken = LoginToken
  { lt        :: String
  , execution :: String
  }
  deriving (Eq, Show, Read, Generic)

instance FromJSON LoginToken
instance ToJSON LoginToken


decode :: LBS.ByteString -> Maybe LoginToken
decode = Aeson.decode


encode :: LoginToken -> LBS.ByteString
encode = Aeson.encode


-- vim:sw=2
