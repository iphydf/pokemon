{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Pokemon.LoginRequest where

import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.String          (fromString)
import           GHC.Generics         (Generic)

import qualified Pokemon.LoginToken   as LoginToken


data LoginRequest = LoginRequest
  { lt        :: String
  , execution :: String
  , eventId   :: String
  , username  :: String
  , password  :: String
  }
  deriving (Eq, Show, Read, Generic)

instance FromJSON LoginRequest
instance ToJSON LoginRequest


decode :: LBS.ByteString -> Maybe LoginRequest
decode = Aeson.decode


encode :: LoginRequest -> LBS.ByteString
encode = Aeson.encode


create :: LoginToken.LoginToken -> String -> String -> String -> LoginRequest
create token =
  LoginRequest
    (LoginToken.lt token)
    (LoginToken.execution token)


toList :: LoginRequest -> [(BS.ByteString, BS.ByteString)]
toList LoginRequest { lt, execution, eventId, username, password } =
  [ ("lt"       , fromString lt       )
  , ("execution", fromString execution)
  , ("_eventId" , fromString eventId  )
  , ("username" , fromString username )
  , ("password" , fromString password )
  ]


-- vim:sw=2
