{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent           (threadDelay)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Default.Class           (def)
import           Data.ProtoLens               (showMessage)
import           System.Directory             (doesFileExist, getHomeDirectory)
import           Text.Groom                   (groom)

import qualified Pokemon.Game                 as Game
import           Pokemon.Profile              (Profile)
import qualified Pokemon.Proto                as Proto


getMapObjects :: Proto.GetMapObjectsMessage
getMapObjects =
  -- Somewhere in London
  Proto.GetMapObjectsMessage [5221378419832389632] [0] 51.507351 (-0.127758)


getFortDetails :: Proto.FortDetailsMessage
getFortDetails =
  Proto.FortDetailsMessage
    "e4a5b5a63cf34100bd620c598597f21c.12"
    51.507335 (-0.127689)


getPlayer :: Proto.GetPlayerMessage
getPlayer =
  Proto.GetPlayerMessage $ Just $
    Proto.GetPlayerMessage'PlayerLocale "en" "en_GB"


readProfile :: IO Profile
readProfile = do
  profileFile <- (++ "/.pokemon.hs") <$> getHomeDirectory
  exists <- doesFileExist profileFile
  if exists
    then readFile  profileFile >>= readIO
    else writeFile profileFile (groom (def :: Profile) ++ "\n") >> return def


main :: IO ()
main = do
  putStrLn "[=] Reading profile..."
  profile <- readProfile

  putStrLn "[=] Logging in..."
  runResourceT $ Game.run profile $ do
    res <- Game.call getPlayer
    liftIO $ putStrLn $ "----- GetPlayer -----\n" ++ showMessage res
    liftIO $ threadDelay (1000 * 1000) -- 1 second before getting the map
    res <- Game.call getMapObjects
    liftIO $ putStrLn $ "----- MapObjects -----\n" ++ showMessage res

    liftIO $ threadDelay (1000 * 1000 * 10) -- 10 seconds delay between map updates
    (r1, r2, r3, r4, r5, r6, r7) <- Game.call
      ( getPlayer
      , def :: Proto.GetHatchedEggsMessage
      , def :: Proto.GetInventoryMessage
      , def :: Proto.CheckAwardedBadgesMessage
      , def :: Proto.DownloadSettingsMessage
      , getMapObjects
      , getFortDetails
      )
    liftIO $ putStrLn $ "----- Player -----\n" ++ showMessage r1
    liftIO $ putStrLn $ "----- HatchedEggs -----\n" ++ showMessage r2
    liftIO $ putStrLn $ "----- Inventory -----\n" ++ showMessage r3
    liftIO $ putStrLn $ "----- AwardedBadges -----\n" ++ showMessage r4
    liftIO $ putStrLn $ "----- DownloadSettings -----\n" ++ showMessage r5
    liftIO $ putStrLn $ "----- MapObjects -----\n" ++ showMessage r6
    liftIO $ putStrLn $ "----- FortDetails -----\n" ++ showMessage r7


-- vim:sw=2
