{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Pokemon.Envelope where

import qualified Data.Binary          as Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Default.Class   (def)
import           Data.ProtoLens       (encodeMessage)
import           Data.Text            (Text)
import           Data.Time.Clock      (NominalDiffTime)
import           Data.Word            (Word32, Word64)
import           Lens.Family2         ((&), (.~), (^.))

import qualified Pokemon.Encrypt      as Encrypt
import           Pokemon.Location     (Location)
import qualified Pokemon.Location     as Location
import           Pokemon.Proto        (AuthTicket, Request, RequestEnvelope,
                                       RequestEnvelope'AuthInfo (..),
                                       RequestEnvelope'AuthInfo'JWT (..),
                                       Signature, Unknown6, Unknown6'Unknown2,
                                       altitude, authInfo, authTicket,
                                       encryptedSignature, latitude,
                                       locationHash1, locationHash2, longitude,
                                       requestHash, requestId, requestType,
                                       requests, sessionHash, statusCode,
                                       timestamp, timestampSinceStart,
                                       unknown12, unknown2, unknown25, unknown6)


data Auth
  = AccessToken Text
  | AuthTicket AuthTicket


authName :: Auth -> String
authName AccessToken {} = "OAuth access token"
authName AuthTicket  {} = "session ticket"


locationBytes :: Location -> Encrypt.PlainText
locationBytes = Encrypt.PlainText . LBS.toStrict . Binary.encode


generateLocation1 :: BS.ByteString -> Location -> Word64
generateLocation1 bs loc =
  let
    firstHash = Encrypt.xxHash32 0x1B845238 (Encrypt.PlainText bs)
  in
  fromIntegral $ Encrypt.xxHash32 firstHash $ locationBytes loc

generateLocation2 :: Location -> Word64
generateLocation2 loc =
  fromIntegral $ Encrypt.xxHash32 0x1B845238 $ locationBytes loc

generateRequestHash :: BS.ByteString -> Request -> Word64
generateRequestHash ticket request =
  let
    firstHash = Encrypt.xxHash64 0x1B845238 (Encrypt.PlainText ticket)
  in
  Encrypt.xxHash64 firstHash (Encrypt.PlainText $ encodeMessage request)


authenticate
  :: Encrypt.SessionHash
  -> Encrypt.IV
  -> NominalDiffTime
  -> NominalDiffTime
  -> Auth
  -> RequestEnvelope
  -> RequestEnvelope
authenticate _ _ _ _ (AccessToken accessToken) env =
  env & authInfo .~ ptcAuthInfo
  where
    ptcAuthInfo =
      RequestEnvelope'AuthInfo
        "ptc"
        (Just (RequestEnvelope'AuthInfo'JWT
            accessToken
            59))

authenticate sHash iv now startTime (AuthTicket ticket) env =
  env
    & authTicket .~ ticket
    & unknown6   .~ uk6
  where
    lat = env ^. latitude
    lng = env ^. longitude
    alt = env ^. altitude
    loc = Location.fromLatLngAlt lat lng alt

    ticketSerialised = encodeMessage ticket

    uk6 = (def :: Unknown6)
      & requestType .~ 6
      & unknown2 .~ uk2

    uk2 = (def :: Unknown6'Unknown2)
      & encryptedSignature .~ Encrypt.encrypt iv (Encrypt.PlainText sig)

    sig = encodeMessage $ (def :: Signature)
      & locationHash1       .~ generateLocation1 ticketSerialised loc
      & locationHash2       .~ generateLocation2 loc
      & requestHash         .~ map (generateRequestHash ticketSerialised) (env ^. requests)
      & sessionHash         .~ Encrypt.sessionHashToBS sHash
      & timestamp           .~ round (now * 1000)
      & timestampSinceStart .~ round ((now - startTime) * 1000)
      & unknown25           .~ fromIntegral (Encrypt.xxHash64 0x88533787 "\"b8fa9757195897aae92c53dbcf8a60fb3d86d745\"")


envelope
  :: Word64
  -> Encrypt.SessionHash
  -> Encrypt.IV
  -> NominalDiffTime
  -> NominalDiffTime
  -> Auth
  -> Location
  -> [Request]
  -> RequestEnvelope
envelope reqId sHash iv now startTime auth location reqs =
  authenticate sHash iv now startTime auth $ (def :: RequestEnvelope)
    & statusCode .~ 2
    & requestId  .~ reqId
    & requests   .~ reqs
    & latitude   .~ Location.latitude location
    & longitude  .~ Location.longitude location
    & altitude   .~ Location.altitude location
    & unknown12  .~ 989


-- vim:sw=2
