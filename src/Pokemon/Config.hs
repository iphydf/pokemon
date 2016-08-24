{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
module Pokemon.Config where

import           Data.Default.Class        (def)
import           Lens.Family2              ((&), (.~), (^.))
import           Network.HTTP.Conduit      (Request (..))
import           Network.HTTP.Types.Header (Header)

import qualified Pokemon.Proto             as Proto


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


deviceInfo :: Proto.Signature'DeviceInfo
deviceInfo = (def :: Proto.Signature'DeviceInfo)
  & Proto.deviceId              .~ "1e95532dcb0c9eef"
  & Proto.androidBoardName      .~ "MSM8974"
  & Proto.androidBootloader     .~ "G900IDVU1CPG2"
  & Proto.deviceBrand           .~ "samsung"
  & Proto.deviceModel           .~ "klte"
  & Proto.deviceModelIdentifier .~ "MMB29M.G900IDVU1CPG2"
  & Proto.deviceModelBoot       .~ "qcom"
  & Proto.hardwareManufacturer  .~ "samsung"
  & Proto.hardwareModel         .~ "SM-G900I"
  & Proto.firmwareBrand         .~ "kltedv"
  & Proto.firmwareTags          .~ "release-keys"
  & Proto.firmwareType          .~ "user"
  & Proto.firmwareFingerprint   .~ "samsung/kltedv/klte:6.0.1/MMB29M/G900IDVU1CPG2:user/release-keys"


-- vim:sw=2
