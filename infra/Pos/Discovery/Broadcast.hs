{-# LANGUAGE RankNTypes #-}

-- | This module implements the capabilities of broadcasting info to
-- neighbors.
module Pos.Discovery.Broadcast
       ( converseToNeighbors
       ) where


import qualified Data.Set                   as S
import           Formatting                 (int, sformat, (%))
import           Mockable                   (MonadMockable)
import           System.Wlog                (WithLogger, logDebug, logWarning)
import           Universum                  hiding (catchAll)

import           Pos.Communication.Protocol (Conversation, EnqueueMsg,
                                             NodeId, Msg)
import           Pos.Discovery.Class        (MonadDiscovery, getPeers)
import           Pos.Infra.Constants        (neighborsSendThreshold)

check :: (WithLogger m) => Set NodeId -> m (Set NodeId)
check peers = do
    when (S.size peers < neighborsSendThreshold) $
        logWarning $ sformat
            ("Send to only " % int % " nodes, threshold is " % int)
            (S.size peers) (neighborsSendThreshold :: Int)
    return peers

converseToNeighbors
    :: ( MonadMockable m
       , MonadDiscovery m
       , WithLogger m
       )
    => EnqueueMsg m
    -> (Set NodeId -> Msg)
    -> (NodeId -> NonEmpty (Conversation m ()))
    -> m (Map NodeId (m ()))
converseToNeighbors enqueue mkMsg convHandler = do
    peers <- check =<< getPeers
    logDebug $ "converseToNeighbors: sending to nodes: " <> show peers
    t <- enqueue (mkMsg peers) $ \peer _ ->
        convHandler peer
    logDebug "converseToNeighbors: sending to nodes done"
    return t
