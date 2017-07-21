{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RealMode
       , RealModeContext(..)

       , OQ
       , EnqueuedConversation (..)
       ) where

import           Universum

import           Control.Lens                (makeLensesWith)
import qualified Control.Monad.Reader        as Mtl
import           Ether.Internal              (HasLens (..))
import           Mockable                    (Production)
import           Formatting                  (shown)
import           Network.Broadcast.OutboundQueue.Types (FormatMsg (..))
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Node                        as N (Conversation)
import           System.Wlog                 (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener         (MonadBListener (..), onApplyBlocksStub,
                                              onRollbackBlocksStub)
import           Pos.Block.Core              (Block, BlockHeader)
import           Pos.Block.Slog.Types        (HasSlogContext (..))
import           Pos.Block.Types             (Undo)
import           Pos.Communication.Types     (PeerData, PackingType, NodeId, Msg)
import           Pos.Context                 (NodeContext, HasSscContext (..),
                                              HasPrimaryKey (..), HasNodeContext (..))
import           Pos.Core                    (IsHeader)
import           Pos.DB                      (MonadGState (..), NodeDBs)
import           Pos.DB.Block                (dbGetBlockDefault, dbGetBlockSscDefault,
                                              dbGetHeaderDefault, dbGetHeaderSscDefault,
                                              dbGetUndoDefault, dbGetUndoSscDefault,
                                              dbPutBlundDefault)
import           Pos.DB.Class                (MonadBlockDBGeneric (..),
                                              MonadBlockDBGenericWrite (..), MonadDB (..),
                                              MonadDBRead (..))
import           Pos.DB.DB                   (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks                (dbDeleteDefault, dbGetDefault,
                                              dbIterSourceDefault, dbPutDefault,
                                              dbWriteBatchDefault)
import           Pos.Delegation.Class        (DelegationVar)
import           Pos.Discovery               (HasDiscoveryContextSum (..),
                                              MonadDiscovery (..), findPeersSum,
                                              getPeersSum)
import           Pos.Reporting               (HasReportingContext (..))
import           Pos.KnownPeers              (MonadKnownPeers (..))
import           Pos.Shutdown                (HasShutdownContext (..))
import           Pos.Slotting.Class          (MonadSlots (..))
import           Pos.Slotting.Impl.Sum       (currentTimeSlottingSum,
                                              getCurrentSlotBlockingSum,
                                              getCurrentSlotInaccurateSum,
                                              getCurrentSlotSum)
import           Pos.Slotting.MemState       (HasSlottingVar (..), MonadSlotsData (..),
                                              getSlottingDataDefault,
                                              getSystemStartDefault,
                                              putSlottingDataDefault,
                                              waitPenultEpochEqualsDefault)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Ssc.Class.Types         (SscBlock)
import           Pos.Ssc.Extra               (SscMemTag, SscState)
import           Pos.Txp.MemState            (GenericTxpLocalData, TxpHolderTag,
                                              TxpMetrics)
import           Pos.Util                    (Some (..))
import           Pos.Util.JsonLog            (HasJsonLogConfig (..), JsonLogConfig,
                                              jsonLogDefault)
import           Pos.Util.LoggerName         (HasLoggerName' (..), getLoggerNameDefault,
                                              modifyLoggerNameDefault)
import           Pos.Util.TimeWarp           (CanJsonLog (..))
import           Pos.Util.UserSecret         (HasUserSecret (..))
import           Pos.Util.Util               (postfixLFields)
import           Pos.WorkMode.Class          (MinWorkMode, TxpExtra_TMP, WorkMode)

newtype EnqueuedConversation m t =
    EnqueuedConversation (Msg, NodeId -> PeerData -> N.Conversation PackingType m t)

instance FormatMsg (EnqueuedConversation m) where
    formatMsg = (\k (EnqueuedConversation (msg, _)) -> k msg) <$> shown

type OQ m = OQ.OutboundQ (EnqueuedConversation m) NodeId

data RealModeContext ssc = RealModeContext
    { rmcNodeDBs       :: !(NodeDBs)
    , rmcSscState      :: !(SscState ssc)
    , rmcTxpLocalData  :: !(GenericTxpLocalData TxpExtra_TMP, TxpMetrics)
    , rmcDelegationVar :: !DelegationVar
    , rmcJsonLogConfig :: !JsonLogConfig
    , rmcLoggerName    :: !LoggerName
    , rmcNodeContext   :: !(NodeContext ssc)
    , rmcOutboundQ     :: !(OQ (RealMode ssc))
    }

type RealMode ssc = Mtl.ReaderT (RealModeContext ssc) Production

makeLensesWith postfixLFields ''RealModeContext

instance HasLens NodeDBs (RealModeContext ssc) NodeDBs where
    lensOf = rmcNodeDBs_L

instance HasLens SscMemTag (RealModeContext ssc) (SscState ssc) where
    lensOf = rmcSscState_L

instance HasLens TxpHolderTag (RealModeContext ssc) ( GenericTxpLocalData TxpExtra_TMP
                                                    , TxpMetrics) where
    lensOf = rmcTxpLocalData_L

instance HasLens DelegationVar (RealModeContext ssc) DelegationVar where
    lensOf = rmcDelegationVar_L

instance {-# OVERLAPPABLE #-}
    HasLens tag (NodeContext ssc) r =>
    HasLens tag (RealModeContext ssc) r
  where
    lensOf = rmcNodeContext_L . lensOf @tag

instance HasSscContext ssc (RealModeContext ssc) where
    sscContext = rmcNodeContext_L . sscContext

instance HasPrimaryKey (RealModeContext ssc) where
    primaryKey = rmcNodeContext_L . primaryKey

instance HasDiscoveryContextSum (RealModeContext ssc) where
    discoveryContextSum = rmcNodeContext_L . discoveryContextSum

instance HasReportingContext (RealModeContext ssc) where
    reportingContext = rmcNodeContext_L . reportingContext

instance HasUserSecret (RealModeContext ssc) where
    userSecret = rmcNodeContext_L . userSecret

instance HasShutdownContext (RealModeContext ssc) where
    shutdownContext = rmcNodeContext_L . shutdownContext

instance HasSlottingVar (RealModeContext ssc) where
    slottingTimestamp = rmcNodeContext_L . slottingTimestamp
    slottingVar = rmcNodeContext_L . slottingVar

instance HasSlogContext (RealModeContext ssc) where
    slogContextL = rmcNodeContext_L . slogContextL

instance HasNodeContext ssc (RealModeContext ssc) where
    nodeContext = rmcNodeContext_L

instance HasLoggerName' (RealModeContext ssc) where
    loggerName = rmcLoggerName_L

instance HasJsonLogConfig (RealModeContext ssc) where
    jsonLogConfig = rmcJsonLogConfig_L

instance {-# OVERLAPPING #-} HasLoggerName (RealMode ssc) where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog (RealMode ssc) where
    jsonLog = jsonLogDefault

instance MonadSlotsData (RealMode ssc) where
    getSystemStart = getSystemStartDefault
    getSlottingData = getSlottingDataDefault
    waitPenultEpochEquals = waitPenultEpochEqualsDefault
    putSlottingData = putSlottingDataDefault

instance MonadSlots (RealMode ssc) where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

instance MonadDiscovery (RealMode ssc) where
    getPeers = getPeersSum
    findPeers = findPeersSum

instance MonadGState (RealMode ssc) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadDBRead (RealMode ssc) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance MonadDB (RealMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance MonadBListener (RealMode ssc) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockDefault @ssc
    dbGetUndo   = dbGetUndoDefault @ssc
    dbGetHeader = dbGetHeaderDefault @ssc

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockSscDefault @ssc
    dbGetUndo   = dbGetUndoSscDefault @ssc
    dbGetHeader = dbGetHeaderSscDefault @ssc

instance SscHelpersClass ssc =>
         MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo (RealMode ssc) where
    dbPutBlund = dbPutBlundDefault

instance MonadKnownPeers (RealMode ssc) where
    updateKnownPeers f = do
        oq <- rmcOutboundQ <$> ask
        OQ.updateKnownPeers oq f
    addKnownPeers peers = do
        oq <- rmcOutboundQ <$> ask
        OQ.addKnownPeers oq peers
    removeKnownPeer nid = do
        oq <- rmcOutboundQ <$> ask
        OQ.removeKnownPeer oq nid
