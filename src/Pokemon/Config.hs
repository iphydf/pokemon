{-# LANGUAGE OverloadedStrings #-}
module Pokemon.Config where

import           Network.HTTP.Conduit      (Request (..))
import           Network.HTTP.Types.Header (Header)


userAgent :: Header
userAgent = ("User-Agent", "niantic")


setUserAgent :: Request -> Request
setUserAgent req = req { requestHeaders = userAgent : requestHeaders req }


clientSecret :: String
clientSecret = "w8ScCUXJQc6kXKw8FiOhd8Fixzht18Dq3PEVkUCP5ZPxtgyWsbTvWHFLm2wNY0JR"


apiReq :: Request
apiReq = setUserAgent
  "https://pgorelease.nianticlabs.com/plfe/rpc"


loginReq :: Request
loginReq = setUserAgent
  "https://sso.pokemon.com/sso/oauth2.0/authorize?client_id=mobile-app_pokemon-go&redirect_uri=https%3A%2F%2Fwww.nianticlabs.com%2Fpokemongo%2Ferror"


loginOAuth :: Request
loginOAuth = setUserAgent
  "https://sso.pokemon.com/sso/oauth2.0/accessToken"


-- vim:sw=2
