{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Pokemon.AuthRequest where

import qualified Data.ByteString as BS
import           Data.String     (fromString)

import qualified Pokemon.Config  as Config


data AuthRequest = AuthRequest
  { clientId     :: String
  , redirectUri  :: String
  , clientSecret :: String
  , grantType    :: String
  , code         :: String
  }
  deriving (Eq, Show, Read)


create :: String -> AuthRequest
create = AuthRequest
  "mobile-app_pokemon-go"
  "https://www.nianticlabs.com/pokemongo/error"
  Config.clientSecret
  "refresh_token"


toList :: AuthRequest -> [(BS.ByteString, BS.ByteString)]
toList AuthRequest { clientId, redirectUri, clientSecret, grantType, code } =
  [ ("client_id"    , fromString clientId    )
  , ("redirect_uri" , fromString redirectUri )
  , ("client_secret", fromString clientSecret)
  , ("grant_type"   , fromString grantType   )
  , ("code"         , fromString code        )
  ]


-- vim:sw=2
