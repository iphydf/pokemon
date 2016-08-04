{-# LANGUAGE OverloadedStrings #-}
module Pokemon.Game where

import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Default.Class           (Default (..))
import           Data.Text                    (Text)

import           Pokemon.Api                  (Api)
import qualified Pokemon.Api                  as Api
import           Pokemon.Profile              (Profile)
import qualified Pokemon.Rpc                  as Rpc


data Game = Game
  { name :: String
  }


instance Default Game where
  def = Game
    { name = "pokemon"
    }


version :: Text
version = "0.31.0"


call :: (MonadResource m, Rpc.RpcMethod rpc) => rpc -> Api m s (Rpc.Returns rpc)
call = Rpc.typedCall Api.call


run :: MonadResource m => Profile -> Api m Game a -> m a
run = Api.run def


-- vim:sw=2
