{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
module Pokemon.Rpc
  ( RpcMethod
  , Returns
  , typedCall
  ) where

import qualified Data.ByteString as BS
import           Data.ProtoLens  (Message, decodeMessage, encodeMessage)

import qualified Pokemon.Proto   as Proto


class RpcMethod a where
  type Returns a

  requestTypes :: a -> [Proto.RequestType]

  encode :: a -> [BS.ByteString]
  decode :: Monad m => a -> [BS.ByteString] -> m ([BS.ByteString], Returns a)

  default encode
    :: Message a
    => a -> [BS.ByteString]
  encode = (:[]) . encodeMessage

  default decode
    :: (Message (Returns a), Monad m)
    => a -> [BS.ByteString] -> m ([BS.ByteString], Returns a)
  decode _ [] = fail "insufficient results"
  decode _ (m:ms) =
    case decodeMessage m of
      Left err ->
        fail $ "proto decoding error: " ++ err
      Right ok ->
        return (ms, ok)


toRequest :: RpcMethod a => a -> [Proto.Request]
toRequest reqs =
  zipWith Proto.Request (requestTypes reqs) (encode reqs)


typedCall :: (RpcMethod a, Monad m)
          => ([Proto.Request] -> m [BS.ByteString]) -> a -> m (Returns a)
typedCall f reqs =
  snd <$> (f (toRequest reqs) >>= decode reqs)


instance (RpcMethod a1, RpcMethod a2) => RpcMethod (a1, a2) where
  type Returns (a1, a2) = (Returns a1, Returns a2)

  requestTypes (a1, a2) = requestTypes a1 ++ requestTypes a2

  encode (a1, a2) = encode a1 ++ encode a2

  decode (evA1, evA2) ms = do
    (ms, a1) <- decode evA1 ms
    (ms, a2) <- decode evA2 ms
    return (ms, (a1, a2))


instance (RpcMethod a1, RpcMethod a2, RpcMethod a3) => RpcMethod (a1, a2, a3) where
  type Returns (a1, a2, a3) = (Returns a1, Returns a2, Returns a3)

  requestTypes (a1, a2, a3) = requestTypes a1 ++ requestTypes a2 ++ requestTypes a3

  encode (a1, a2, a3) = encode a1 ++ encode a2 ++ encode a3

  decode (evA1, evA2, evA3) ms = do
    (ms, (a1, a2)) <- decode (evA1, evA2) ms
    (ms, a3) <- decode evA3 ms
    return (ms, (a1, a2, a3))


instance (RpcMethod a1, RpcMethod a2, RpcMethod a3, RpcMethod a4) => RpcMethod (a1, a2, a3, a4) where
  type Returns (a1, a2, a3, a4) = (Returns a1, Returns a2, Returns a3, Returns a4)

  requestTypes (a1, a2, a3, a4) = requestTypes a1 ++ requestTypes a2 ++ requestTypes a3 ++ requestTypes a4

  encode (a1, a2, a3, a4) = encode a1 ++ encode a2 ++ encode a3 ++ encode a4

  decode (evA1, evA2, evA3, evA4) ms = do
    (ms, (a1, a2, a3)) <- decode (evA1, evA2, evA3) ms
    (ms, a4) <- decode evA4 ms
    return (ms, (a1, a2, a3, a4))


instance (RpcMethod a1, RpcMethod a2, RpcMethod a3, RpcMethod a4, RpcMethod a5) => RpcMethod (a1, a2, a3, a4, a5) where
  type Returns (a1, a2, a3, a4, a5) = (Returns a1, Returns a2, Returns a3, Returns a4, Returns a5)

  requestTypes (a1, a2, a3, a4, a5) = requestTypes a1 ++ requestTypes a2 ++ requestTypes a3 ++ requestTypes a4 ++ requestTypes a5

  encode (a1, a2, a3, a4, a5) = encode a1 ++ encode a2 ++ encode a3 ++ encode a4 ++ encode a5

  decode (evA1, evA2, evA3, evA4, evA5) ms = do
    (ms, (a1, a2, a3, a4)) <- decode (evA1, evA2, evA3, evA4) ms
    (ms, a5) <- decode evA5 ms
    return (ms, (a1, a2, a3, a4, a5))


instance (RpcMethod a1, RpcMethod a2, RpcMethod a3, RpcMethod a4, RpcMethod a5, RpcMethod a6) => RpcMethod (a1, a2, a3, a4, a5, a6) where
  type Returns (a1, a2, a3, a4, a5, a6) = (Returns a1, Returns a2, Returns a3, Returns a4, Returns a5, Returns a6)

  requestTypes (a1, a2, a3, a4, a5, a6) = requestTypes a1 ++ requestTypes a2 ++ requestTypes a3 ++ requestTypes a4 ++ requestTypes a5 ++ requestTypes a6

  encode (a1, a2, a3, a4, a5, a6) = encode a1 ++ encode a2 ++ encode a3 ++ encode a4 ++ encode a5 ++ encode a6

  decode (evA1, evA2, evA3, evA4, evA5, evA6) ms = do
    (ms, (a1, a2, a3, a4, a5)) <- decode (evA1, evA2, evA3, evA4, evA5) ms
    (ms, a6) <- decode evA6 ms
    return (ms, (a1, a2, a3, a4, a5, a6))


instance (RpcMethod a1, RpcMethod a2, RpcMethod a3, RpcMethod a4, RpcMethod a5, RpcMethod a6, RpcMethod a7) => RpcMethod (a1, a2, a3, a4, a5, a6, a7) where
  type Returns (a1, a2, a3, a4, a5, a6, a7) = (Returns a1, Returns a2, Returns a3, Returns a4, Returns a5, Returns a6, Returns a7)

  requestTypes (a1, a2, a3, a4, a5, a6, a7) = requestTypes a1 ++ requestTypes a2 ++ requestTypes a3 ++ requestTypes a4 ++ requestTypes a5 ++ requestTypes a6 ++ requestTypes a7

  encode (a1, a2, a3, a4, a5, a6, a7) = encode a1 ++ encode a2 ++ encode a3 ++ encode a4 ++ encode a5 ++ encode a6 ++ encode a7

  decode (evA1, evA2, evA3, evA4, evA5, evA6, evA7) ms = do
    (ms, (a1, a2, a3, a4, a5, a6)) <- decode (evA1, evA2, evA3, evA4, evA5, evA6) ms
    (ms, a7) <- decode evA7 ms
    return (ms, (a1, a2, a3, a4, a5, a6, a7))


instance RpcMethod Proto.AddFortModifierMessage where
  type Returns Proto.AddFortModifierMessage = Proto.AddFortModifierResponse
  requestTypes _ = [Proto.ADD_FORT_MODIFIER]

instance RpcMethod Proto.AttackGymMessage where
  type Returns Proto.AttackGymMessage = Proto.AttackGymResponse
  requestTypes _ = [Proto.ATTACK_GYM]

instance RpcMethod Proto.CatchPokemonMessage where
  type Returns Proto.CatchPokemonMessage = Proto.CatchPokemonResponse
  requestTypes _ = [Proto.CATCH_POKEMON]

instance RpcMethod Proto.CheckAwardedBadgesMessage where
  type Returns Proto.CheckAwardedBadgesMessage = Proto.CheckAwardedBadgesResponse
  requestTypes _ = [Proto.CHECK_AWARDED_BADGES]

instance RpcMethod Proto.CheckCodenameAvailableMessage where
  type Returns Proto.CheckCodenameAvailableMessage = Proto.CheckCodenameAvailableResponse
  requestTypes _ = [Proto.CHECK_CODENAME_AVAILABLE]

instance RpcMethod Proto.ClaimCodenameMessage where
  type Returns Proto.ClaimCodenameMessage = Proto.ClaimCodenameResponse
  requestTypes _ = [Proto.CLAIM_CODENAME]

instance RpcMethod Proto.CollectDailyBonusMessage where
  type Returns Proto.CollectDailyBonusMessage = Proto.CollectDailyBonusResponse
  requestTypes _ = [Proto.COLLECT_DAILY_BONUS]

instance RpcMethod Proto.CollectDailyDefenderBonusMessage where
  type Returns Proto.CollectDailyDefenderBonusMessage = Proto.CollectDailyDefenderBonusResponse
  requestTypes _ = [Proto.COLLECT_DAILY_DEFENDER_BONUS]

instance RpcMethod Proto.DiskEncounterMessage where
  type Returns Proto.DiskEncounterMessage = Proto.DiskEncounterResponse
  requestTypes _ = [Proto.DISK_ENCOUNTER]

instance RpcMethod Proto.DownloadItemTemplatesMessage where
  type Returns Proto.DownloadItemTemplatesMessage = Proto.DownloadItemTemplatesResponse
  requestTypes _ = [Proto.DOWNLOAD_ITEM_TEMPLATES]

instance RpcMethod Proto.DownloadRemoteConfigVersionMessage where
  type Returns Proto.DownloadRemoteConfigVersionMessage = Proto.DownloadRemoteConfigVersionResponse
  requestTypes _ = [Proto.DOWNLOAD_REMOTE_CONFIG_VERSION]

instance RpcMethod Proto.DownloadSettingsMessage where
  type Returns Proto.DownloadSettingsMessage = Proto.DownloadSettingsResponse
  requestTypes _ = [Proto.DOWNLOAD_SETTINGS]

instance RpcMethod Proto.EchoMessage where
  type Returns Proto.EchoMessage = Proto.EchoResponse
  requestTypes _ = [Proto.ECHO]

instance RpcMethod Proto.EncounterMessage where
  type Returns Proto.EncounterMessage = Proto.EncounterResponse
  requestTypes _ = [Proto.ENCOUNTER]

instance RpcMethod Proto.EncounterTutorialCompleteMessage where
  type Returns Proto.EncounterTutorialCompleteMessage = Proto.EncounterTutorialCompleteResponse
  requestTypes _ = [Proto.ENCOUNTER_TUTORIAL_COMPLETE]

instance RpcMethod Proto.EquipBadgeMessage where
  type Returns Proto.EquipBadgeMessage = Proto.EquipBadgeResponse
  requestTypes _ = [Proto.EQUIP_BADGE]

instance RpcMethod Proto.EvolvePokemonMessage where
  type Returns Proto.EvolvePokemonMessage = Proto.EvolvePokemonResponse
  requestTypes _ = [Proto.EVOLVE_POKEMON]

instance RpcMethod Proto.FortDeployPokemonMessage where
  type Returns Proto.FortDeployPokemonMessage = Proto.FortDeployPokemonResponse
  requestTypes _ = [Proto.FORT_DEPLOY_POKEMON]

instance RpcMethod Proto.FortDetailsMessage where
  type Returns Proto.FortDetailsMessage = Proto.FortDetailsResponse
  requestTypes _ = [Proto.FORT_DETAILS]

instance RpcMethod Proto.FortRecallPokemonMessage where
  type Returns Proto.FortRecallPokemonMessage = Proto.FortRecallPokemonResponse
  requestTypes _ = [Proto.FORT_RECALL_POKEMON]

instance RpcMethod Proto.FortSearchMessage where
  type Returns Proto.FortSearchMessage = Proto.FortSearchResponse
  requestTypes _ = [Proto.FORT_SEARCH]

instance RpcMethod Proto.GetAssetDigestMessage where
  type Returns Proto.GetAssetDigestMessage = Proto.GetAssetDigestResponse
  requestTypes _ = [Proto.GET_ASSET_DIGEST]

instance RpcMethod Proto.GetDownloadUrlsMessage where
  type Returns Proto.GetDownloadUrlsMessage = Proto.GetDownloadUrlsResponse
  requestTypes _ = [Proto.GET_DOWNLOAD_URLS]

instance RpcMethod Proto.GetGymDetailsMessage where
  type Returns Proto.GetGymDetailsMessage = Proto.GetGymDetailsResponse
  requestTypes _ = [Proto.GET_GYM_DETAILS]

instance RpcMethod Proto.GetHatchedEggsMessage where
  type Returns Proto.GetHatchedEggsMessage = Proto.GetHatchedEggsResponse
  requestTypes _ = [Proto.GET_HATCHED_EGGS]

instance RpcMethod Proto.GetIncensePokemonMessage where
  type Returns Proto.GetIncensePokemonMessage = Proto.GetIncensePokemonResponse
  requestTypes _ = [Proto.GET_INCENSE_POKEMON]

instance RpcMethod Proto.GetInventoryMessage where
  type Returns Proto.GetInventoryMessage = Proto.GetInventoryResponse
  requestTypes _ = [Proto.GET_INVENTORY]

instance RpcMethod Proto.GetMapObjectsMessage where
  type Returns Proto.GetMapObjectsMessage = Proto.GetMapObjectsResponse
  requestTypes _ = [Proto.GET_MAP_OBJECTS]

instance RpcMethod Proto.GetPlayerMessage where
  type Returns Proto.GetPlayerMessage = Proto.GetPlayerResponse
  requestTypes _ = [Proto.GET_PLAYER]

instance RpcMethod Proto.GetPlayerProfileMessage where
  type Returns Proto.GetPlayerProfileMessage = Proto.GetPlayerProfileResponse
  requestTypes _ = [Proto.GET_PLAYER_PROFILE]

instance RpcMethod Proto.GetSuggestedCodenamesMessage where
  type Returns Proto.GetSuggestedCodenamesMessage = Proto.GetSuggestedCodenamesResponse
  requestTypes _ = [Proto.GET_SUGGESTED_CODENAMES]

instance RpcMethod Proto.IncenseEncounterMessage where
  type Returns Proto.IncenseEncounterMessage = Proto.IncenseEncounterResponse
  requestTypes _ = [Proto.INCENSE_ENCOUNTER]

instance RpcMethod Proto.LevelUpRewardsMessage where
  type Returns Proto.LevelUpRewardsMessage = Proto.LevelUpRewardsResponse
  requestTypes _ = [Proto.LEVEL_UP_REWARDS]

instance RpcMethod Proto.MarkTutorialCompleteMessage where
  type Returns Proto.MarkTutorialCompleteMessage = Proto.MarkTutorialCompleteResponse
  requestTypes _ = [Proto.MARK_TUTORIAL_COMPLETE]

instance RpcMethod Proto.NicknamePokemonMessage where
  type Returns Proto.NicknamePokemonMessage = Proto.NicknamePokemonResponse
  requestTypes _ = [Proto.NICKNAME_POKEMON]

instance RpcMethod Proto.PlayerUpdateMessage where
  type Returns Proto.PlayerUpdateMessage = Proto.PlayerUpdateResponse
  requestTypes _ = [Proto.PLAYER_UPDATE]

instance RpcMethod Proto.RecycleInventoryItemMessage where
  type Returns Proto.RecycleInventoryItemMessage = Proto.RecycleInventoryItemResponse
  requestTypes _ = [Proto.RECYCLE_INVENTORY_ITEM]

instance RpcMethod Proto.ReleasePokemonMessage where
  type Returns Proto.ReleasePokemonMessage = Proto.ReleasePokemonResponse
  requestTypes _ = [Proto.RELEASE_POKEMON]

instance RpcMethod Proto.SetAvatarMessage where
  type Returns Proto.SetAvatarMessage = Proto.SetAvatarResponse
  requestTypes _ = [Proto.SET_AVATAR]

instance RpcMethod Proto.SetContactSettingsMessage where
  type Returns Proto.SetContactSettingsMessage = Proto.SetContactSettingsResponse
  requestTypes _ = [Proto.SET_CONTACT_SETTINGS]

instance RpcMethod Proto.SetFavoritePokemonMessage where
  type Returns Proto.SetFavoritePokemonMessage = Proto.SetFavoritePokemonResponse
  requestTypes _ = [Proto.SET_FAVORITE_POKEMON]

instance RpcMethod Proto.SetPlayerTeamMessage where
  type Returns Proto.SetPlayerTeamMessage = Proto.SetPlayerTeamResponse
  requestTypes _ = [Proto.SET_PLAYER_TEAM]

instance RpcMethod Proto.SfidaActionLogMessage where
  type Returns Proto.SfidaActionLogMessage = Proto.SfidaActionLogResponse
  requestTypes _ = [Proto.SFIDA_ACTION_LOG]

instance RpcMethod Proto.StartGymBattleMessage where
  type Returns Proto.StartGymBattleMessage = Proto.StartGymBattleResponse
  requestTypes _ = [Proto.START_GYM_BATTLE]

instance RpcMethod Proto.UpgradePokemonMessage where
  type Returns Proto.UpgradePokemonMessage = Proto.UpgradePokemonResponse
  requestTypes _ = [Proto.UPGRADE_POKEMON]

instance RpcMethod Proto.UseIncenseMessage where
  type Returns Proto.UseIncenseMessage = Proto.UseIncenseResponse
  requestTypes _ = [Proto.USE_INCENSE]

instance RpcMethod Proto.UseItemCaptureMessage where
  type Returns Proto.UseItemCaptureMessage = Proto.UseItemCaptureResponse
  requestTypes _ = [Proto.USE_ITEM_CAPTURE]

instance RpcMethod Proto.UseItemEggIncubatorMessage where
  type Returns Proto.UseItemEggIncubatorMessage = Proto.UseItemEggIncubatorResponse
  requestTypes _ = [Proto.USE_ITEM_EGG_INCUBATOR]

instance RpcMethod Proto.UseItemGymMessage where
  type Returns Proto.UseItemGymMessage = Proto.UseItemGymResponse
  requestTypes _ = [Proto.USE_ITEM_GYM]

instance RpcMethod Proto.UseItemPotionMessage where
  type Returns Proto.UseItemPotionMessage = Proto.UseItemPotionResponse
  requestTypes _ = [Proto.USE_ITEM_POTION]

instance RpcMethod Proto.UseItemReviveMessage where
  type Returns Proto.UseItemReviveMessage = Proto.UseItemReviveResponse
  requestTypes _ = [Proto.USE_ITEM_REVIVE]

instance RpcMethod Proto.UseItemXpBoostMessage where
  type Returns Proto.UseItemXpBoostMessage = Proto.UseItemXpBoostResponse
  requestTypes _ = [Proto.USE_ITEM_XP_BOOST]


-- vim:sw=2
