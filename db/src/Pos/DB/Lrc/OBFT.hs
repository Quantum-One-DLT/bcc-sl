module Pos.DB.Lrc.SBFT
       ( getSlotLeaderSbft
       , getEpochSlotLeaderScheduleSbft
       ) where

import           Universum

import           Pos.Chain.Delegation (ProxySKBlockInfo)
import           Pos.Chain.Genesis (configGenesisWStakeholders)
import qualified Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Lrc (getEpochSlotLeaderScheduleSbftPure,
                     getSlotLeaderSbftPure)
import           Pos.Core (EpochIndex, SlotCount (..), SlotId (..), SlotLeaders,
                     StakeholderId, pcEpochSlots)
import           Pos.DB (MonadDBRead)
import           Pos.DB.Delegation (getDlgTransPsk)

import           UnliftIO (MonadUnliftIO)

-- | This function selects the current slot leaders by obtaining the
-- genesis stakeholders, then tracing them through the delegation
-- mapping.
getSlotLeaderSbft
    :: (MonadDBRead m, MonadUnliftIO m)
    => Genesis.Config -> SlotId -> m (StakeholderId, ProxySKBlockInfo)
getSlotLeaderSbft genesisConfig si = do
    mDlg <- getDlgTransPsk currentSlotGenesisSId
    pure (currentSlotGenesisSId, (swap <$> mDlg))
  where
    -- We assume here that the genesis bootstrap stakeholders list
    -- is nonempty
    stakeholders :: [StakeholderId]
    stakeholders = sort $ configGenesisWStakeholders genesisConfig
    --
    epochSlotCount :: SlotCount
    epochSlotCount =
        pcEpochSlots (Genesis.configProtocolConstants genesisConfig)
    --
    currentSlotGenesisSId :: StakeholderId
    currentSlotGenesisSId =
        case (nonEmpty stakeholders) of
            Just s  -> getSlotLeaderSbftPure si epochSlotCount s
            Nothing -> error "getSlotLeaderSbft: Empty list of stakeholders"

-- | Generates the full slot leader schedule for an epoch (10*k slots long).
getEpochSlotLeaderScheduleSbft
    :: Genesis.Config -> EpochIndex -> SlotLeaders
getEpochSlotLeaderScheduleSbft genesisConfig ei =
    case nonEmpty stakeholders of
        Just s  -> getEpochSlotLeaderScheduleSbftPure ei epochSlotCount s
        Nothing -> error "getEpochSlotLeaderScheduleSbft: Empty list of stakeholders"
  where
    -- We assume here that the genesis bootstrap stakeholders list
    -- is nonempty
    stakeholders :: [StakeholderId]
    stakeholders = sort $ configGenesisWStakeholders genesisConfig
    epochSlotCount = pcEpochSlots (Genesis.configProtocolConstants genesisConfig)
