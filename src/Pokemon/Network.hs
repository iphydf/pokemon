{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Pokemon.Network where

import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Lazy         as LBS
import           Data.ProtoLens               (decodeMessage, encodeMessage)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method    (methodPost)

import           Pokemon.Proto                (RequestEnvelope,
                                               ResponseEnvelope)


call :: (MonadResource m)
     => Manager -> Request -> RequestEnvelope -> m ResponseEnvelope
call manager endpoint request = do
  res <- decodeMessage . LBS.toStrict . responseBody <$> httpLbs req manager
  getRight res
  where
    req = endpoint
      { method      = methodPost
      , requestBody = RequestBodyBS $ encodeMessage request
      }

    getRight (Left err) = fail err
    getRight (Right ok) = return ok


-- vim:sw=2
