module Pos.Chain.Lrc.SBFT
        ( getSlotLeaderSbftPure
        , getEpochSlotLeaderScheduleSbftPure
        ) where

import           Universum hiding (sort)

import           Data.List.NonEmpty ((!!))
import qualified Data.List.NonEmpty as NE (iterate, sort, take)

import           Pos.Core (EpochIndex, FlatSlotId, LocalSlotIndex (..),
                     SlotCount (..), SlotId (..), SlotLeaders, StakeholderId,
                     flattenEpochOrSlot, slotIdSucc)

-- | Selects the StakeholderId that matches the @SlotId@ index in a
-- @SlotCount@-length epoch.
getSlotLeaderSbftPure
    :: SlotId
    -> SlotCount
    -> NonEmpty StakeholderId
    -> StakeholderId
getSlotLeaderSbftPure slotId slotCount stakeholders =
    sortedStakeholders !! leaderIndex
  where
    -- Ensure the stakeholders are sorted
    sortedStakeholders :: NonEmpty StakeholderId
    sortedStakeholders = NE.sort stakeholders
    --
    leaderIndex :: Int
    leaderIndex = (fromIntegral flatSlotId :: Int) `mod` (length stakeholders)
    --
    flatSlotId :: FlatSlotId
    flatSlotId = flattenEpochOrSlot slotCount slotId

-- | Selects @SlotCount@ StakeholderIds for the given epoch @EpochIndex@.
getEpochSlotLeaderScheduleSbftPure
    :: EpochIndex
    -> SlotCount
    -> NonEmpty StakeholderId
    -> SlotLeaders
getEpochSlotLeaderScheduleSbftPure epochIndex epochSlotCount stakeholders =
    case slotLeaderSchedule of
        []  -> error "getEpochSlotLeaderScheduleSbftPure: Empty slot leader schedule"
        sls -> sls
  where
    slotLeaderSchedule =
        map (\si -> getSlotLeaderSbftPure si epochSlotCount stakeholders)
            (NE.take (fromIntegral $ numEpochSlots)
                     (NE.iterate (slotIdSucc epochSlotCount) startSlotId))
    --
    startSlotId :: SlotId
    startSlotId = SlotId epochIndex (UnsafeLocalSlotIndex 0)
    --
    numEpochSlots :: Word64
    numEpochSlots = getSlotCount $ epochSlotCount
