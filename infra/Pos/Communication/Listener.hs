{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Listener
       ( listenerConv
       ) where

import qualified Node                           as N
import           System.Wlog                    (WithLogger)
import           Universum

import           Mockable.Class                 (Mockable)
import           Mockable.Exception             (Throw)
import           Mockable.SharedAtomic          (SharedAtomic)
import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Infra               ()
import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.PeerState    (WithPeerState)
import           Pos.Communication.Protocol     (ConversationActions, HandlerSpec (..),
                                                 ListenerSpec (..), Message, NodeId,
                                                 OutSpecs, VerInfo, checkingInSpecs,
                                                 messageName, convertSendActions, SendActions)
import           Pos.DB.Class                   (MonadGState)

-- TODO automatically provide a 'recvLimited' here by using the
-- 'MessageLimited'?
listenerConv
    :: forall snd rcv m .
       ( Bi snd
       , Bi rcv
       , Message snd
       , Message rcv
       , MonadGState m
       , MessageLimited rcv
       , WithLogger m
       , WithPeerState m
       , Mockable Throw m
       , Mockable SharedAtomic m
       )
    => (VerInfo -> NodeId -> SendActions m -> ConversationActions snd rcv m -> m ())
    -> (ListenerSpec m, OutSpecs)
listenerConv h = (lspec, mempty)
  where
    spec = (rcvMsgName, ConvHandler sndMsgName)
    lspec =
      flip ListenerSpec spec $ \ourVerInfo ->
          N.Listener $ \peerVerInfo' nNodeId sendActions conv -> do
              checkingInSpecs ourVerInfo peerVerInfo' spec nNodeId $
                  h ourVerInfo nNodeId (convertSendActions ourVerInfo sendActions) conv

    sndProxy :: Proxy snd
    sndProxy = Proxy
    rcvProxy :: Proxy rcv
    rcvProxy = Proxy

    sndMsgName = messageName sndProxy
    rcvMsgName = messageName rcvProxy
