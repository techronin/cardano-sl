{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Framework for Inv\/Req\/Data message handling

module Pos.Communication.Relay.Logic
       ( Relay (..)
       , InvMsg (..)
       , ReqMsg (..)
       , MempoolMsg (..)
       , DataMsg (..)
       , relayListeners
       , relayMsg
       , propagateData
       , relayPropagateOut
       , InvOrData
       , handleDataDo
       , handleInvDo

       , invReqDataFlow
       , invReqDataFlowTK
       , invReqDataFlowNeighbors
       , invReqDataFlowNeighborsTK
       , dataFlow
       , InvReqDataFlowLog (..)
       ) where

import           Data.Aeson.TH                      (defaultOptions, deriveJSON)
import           Data.Proxy                         (asProxyTypeOf)
import qualified Data.Set                           as S
import           Data.Tagged                        (Tagged, tagWith)
import           Data.Typeable                      (typeRep)
import           Formatting                         (build, sformat, shown, stext, (%))
import           Mockable                           (MonadMockable, handleAll)
import           Node.Message.Class                 (Message)
import           Node                               (waitForConversations)
import           System.Wlog                        (WithLogger, logDebug,
                                                     logWarning)
import           Universum

import           Pos.Binary.Class                   (Bi (..))
import           Pos.Communication.Limits.Instances ()
import           Pos.Communication.Limits.Types     (MessageLimited, recvLimited)
import           Pos.Communication.Listener         (listenerConv)
import           Pos.Communication.Protocol         (Conversation (..),
                                                     ConversationActions (..),
                                                     ListenerSpec, MkListeners, NodeId,
                                                     OutSpecs, SendActions (..),
                                                     enqueueConversation',
                                                     constantListeners, convH,
                                                     toOutSpecs, Msg, MsgType,
                                                     forwardMsg, sendMsg)
import           Pos.Communication.Relay.Class      (DataParams (..),
                                                     InvReqDataParams (..),
                                                     MempoolParams (..),
                                                     Relay (..))
import           Pos.Communication.Relay.Types      (PropagationMsg (..))
import           Pos.Communication.Relay.Util       (expectData, expectInv)
import           Pos.Communication.Types.Relay      (DataMsg (..), InvMsg (..), InvOrData,
                                                     MempoolMsg (..), ReqMsg (..))
import           Pos.DB.Class                       (MonadGState)
import           Pos.Discovery.Broadcast            (converseToNeighbors)
import           Pos.Discovery.Class                (MonadDiscovery)
import           Pos.Util.TimeWarp                  (CanJsonLog (..))

type MinRelayWorkMode m =
    ( WithLogger m
    , CanJsonLog m
    , MonadMockable m
    , MonadIO m
    )

type RelayWorkMode ctx m =
    ( MinRelayWorkMode m
    )

handleReqL
    :: forall key contents m .
       ( Bi (ReqMsg key)
       , Bi (InvOrData key contents)
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGState m
       )
    => (NodeId -> key -> m (Maybe contents))
    -> (ListenerSpec m, OutSpecs)
handleReqL handleReq = listenerConv $ \__ourVerInfo nodeId _ conv ->
    let handlingLoop = do
            mbMsg <- recvLimited conv
            whenJust mbMsg $ \ReqMsg{..} -> do
                dtMB <- handleReq nodeId rmKey
                case dtMB of
                    Nothing -> logNoData rmKey
                    Just dt -> logHaveData rmKey >> send conv (constructDataMsg dt)
                handlingLoop
    in handlingLoop
  where
    constructDataMsg :: contents -> InvOrData key contents
    constructDataMsg = Right . DataMsg
    logNoData rmKey = logDebug $ sformat
        ("We don't have data for key "%build)
        rmKey
    logHaveData rmKey= logDebug $ sformat
        ("We have data for key "%build)
        rmKey

handleMempoolL
    :: forall m.
       ( MinRelayWorkMode m
       , MonadGState m
       )
    => MempoolParams m
    -> [(ListenerSpec m, OutSpecs)]
handleMempoolL NoMempool = []
handleMempoolL (KeyMempool tagP handleMempool) = pure $ listenerConv $
    \__ourVerInfo __nodeId _ conv -> do
        mbMsg <- recvLimited conv
        whenJust mbMsg $ \msg@MempoolMsg -> do
            let _ = msg `asProxyTypeOf` mmP
            res <- handleMempool
            case nonEmpty res of
                Nothing ->
                    logDebug $ sformat
                        ("We don't have mempool data "%shown) (typeRep tagP)
                Just xs -> do
                    logDebug $ sformat ("We have mempool data "%shown) (typeRep tagP)
                    mapM_ (send conv . InvMsg) xs
  where
    mmP = (const Proxy :: Proxy tag -> Proxy (MempoolMsg tag)) tagP

handleDataOnlyL
    :: forall contents ctx m .
       ( Bi (DataMsg contents)
       , Message Void
       , Message (DataMsg contents)
       , Buildable contents
       , RelayWorkMode ctx m
       , MonadGState m
       , MessageLimited (DataMsg contents)
       , MonadDiscovery m
       )
    => MsgType
    -> (SendActions m -> NodeId -> contents -> m Bool)
    -> (ListenerSpec m, OutSpecs)
handleDataOnlyL msgType handleData = listenerConv $ \__ourVerInfo nodeId sendActions conv ->
    -- First binding is to inform GHC that the send type is Void.
    let _ = send conv :: Void -> m ()
        handlingLoop = do
            mbMsg <- recvLimited conv
            whenJust mbMsg $ \DataMsg{..} -> do
                ifM (handleData sendActions nodeId dmContents)
                    (void $ propagateData sendActions (forwardMsg nodeId) $ DataOnlyPM msgType dmContents)
                    (logUseless dmContents)
                handlingLoop
    in handlingLoop
  where
    logUseless dmContents = logWarning $ sformat
        ("Ignoring data "%build) dmContents

handleDataDo
    :: forall key contents ctx m .
       ( RelayWorkMode ctx m
       , Buildable key
       , Eq key
       , Buildable contents
       , Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , MonadDiscovery m
       , Message Void
       )
    => NodeId
    -> MsgType
    -> SendActions m
    -> (contents -> m key)
    -> (contents -> m Bool)
    -> contents
    -> m ()
handleDataDo provenance msgType sendActions contentsToKey handleData dmContents = do
    dmKey <- contentsToKey dmContents
    ifM (handleData dmContents)
        -- IMPORTANT that we propagate it asynchronously.
        -- enqueueConversation can do that: simply don't force the values in
        -- the resulting map.
        (void $ propagateData sendActions (forwardMsg provenance) $ InvReqDataPM msgType dmKey dmContents) $
            logDebug $ sformat
                ("Ignoring data "%build%" for key "%build) dmContents dmKey

-- | Synchronously propagate data.
relayMsg
    :: ( RelayWorkMode ctx m
       , Message Void
       , MonadDiscovery m
       )
    => SendActions m
    -> (MsgType -> Msg)
    -> PropagationMsg
    -> m ()
relayMsg sendActions mkMsg pm = void $ propagateData sendActions mkMsg pm >>= waitForConversations

-- | Asynchronously propagate data.
propagateData
    :: forall ctx m.
       ( RelayWorkMode ctx m
       , MonadDiscovery m
       , Message Void
       )
    => SendActions m
    -> (MsgType -> Msg)
    -> PropagationMsg
    -> m (Map NodeId (m ()))
propagateData sendActions mkMsg pm = case pm of
    InvReqDataPM msgType key contents -> do
        logDebug $ sformat
            ("Propagation data with key: "%build) key
        converseToNeighbors sendActions (mkMsg msgType) $ \__node ->
            pure $ Conversation $ irdHandler key contents
    DataOnlyPM msgType contents -> do
        logDebug $ sformat
            ("Propagation data: "%build) contents
        converseToNeighbors sendActions (mkMsg msgType) $ \__node ->
            pure $ Conversation $ doHandler contents

  where

    doHandler
        :: contents1
        -> ConversationActions
             (DataMsg contents1) Void m
        -> m ()
    doHandler contents conv = send conv $ DataMsg contents

    irdHandler
        :: Eq key1 => key1 -> contents1
        -> ConversationActions
             (InvOrData key1 contents1) (ReqMsg key1) m
        -> m ()
    irdHandler key conts conv = do
        send conv $ Left $ InvMsg key
        let whileNotK = do
              rm <- recv conv maxBound
              whenJust rm $ \ReqMsg{..} -> do
                if rmKey == key
                   then send conv $ Right $ DataMsg conts
                   else whileNotK
        whileNotK


handleInvDo
    :: forall key ctx m .
       ( RelayWorkMode ctx m
       , Buildable key
       )
    => (key -> m Bool)
    -> key
    -> m (Maybe key)
handleInvDo handleInv imKey =
    ifM (handleInv imKey)
        (Just imKey <$ logUseful)
        (Nothing <$ logUseless)
  where
    logUseless = logDebug $ sformat
        ("Ignoring inv for key "%build%", because it's useless")
        imKey
    logUseful = logDebug $ sformat
        ("We'll request data for key "%build%", because it's useful")
        imKey

relayListenersOne
  :: forall ctx m.
     ( WithLogger m
     , RelayWorkMode ctx m
     , MonadGState m
     , Message Void
     , MonadDiscovery m
     )
  => Relay m -> MkListeners m
relayListenersOne (InvReqData mP irdP@InvReqDataParams{..}) =
    constantListeners $
    [handleReqL handleReq, invDataListener irdP] ++ handleMempoolL mP
relayListenersOne (Data DataParams{..}) =
    constantListeners $
    [handleDataOnlyL dataMsgType handleDataOnly]

relayListeners
  :: forall ctx m.
     ( WithLogger m
     , RelayWorkMode ctx m
     , MonadGState m
     , MonadDiscovery m
     , Message Void
     )
  => [Relay m] -> MkListeners m
relayListeners = mconcat . map relayListenersOne

invDataListener
  :: forall key contents ctx m.
     ( RelayWorkMode ctx m
     , MonadGState m
     , Message (ReqMsg key)
     , Message (InvOrData key contents)
     , Bi (ReqMsg key)
     , Bi (InvOrData key contents)
     , Buildable contents
     , Buildable key
     , Eq key
     , MessageLimited (DataMsg contents)
     , MonadDiscovery m
     , Message Void
     )
  => InvReqDataParams key contents m
  -> (ListenerSpec m, OutSpecs)
invDataListener InvReqDataParams{..} = listenerConv $ \__ourVerInfo nodeId sendActions conv ->
    let handlingLoop = do
            inv' <- recvLimited conv
            whenJust inv' $ expectInv $ \InvMsg{..} -> do
                useful <- handleInvDo (handleInv nodeId) imKey
                whenJust useful $ \ne -> do
                    send conv $ ReqMsg ne
                    dt' <- recvLimited conv
                    whenJust dt' $ expectData $ \DataMsg{..} -> do
                          handleDataDo nodeId invReqMsgType sendActions contentsToKey (handleData nodeId) dmContents
                          -- handlingLoop

                          -- TODO CSL-1148 Improve relaing: support multiple data
                          -- Need to receive Inv and Data messages simultaneously
                          -- Maintain state of sent Reqs
                          -- And check data we are sent is what we expect (currently not)
    in handlingLoop

relayPropagateOut :: Message Void => [Relay m] -> OutSpecs
relayPropagateOut = mconcat . map propagateOutImpl

propagateOutImpl :: Message Void => Relay m -> OutSpecs
propagateOutImpl (InvReqData _ irdp) = toOutSpecs
      [ convH invProxy reqProxy
      ]
  where
    invProxy = (const Proxy :: InvReqDataParams key contents m
                            -> Proxy (InvOrData key contents)) irdp
    reqProxy = (const Proxy :: InvReqDataParams key contents m
                            -> Proxy (ReqMsg key)) irdp
propagateOutImpl (Data dp) = toOutSpecs
      [ convH dataProxy (Proxy @Void)
      ]
  where
    dataProxy = (const Proxy :: DataParams contents m
                            -> Proxy (DataMsg contents)) dp

----------------------------------------------------------------------------
-- Helpers for Communication.Methods
----------------------------------------------------------------------------

data InvReqDataFlowLog =
      InvReqAccepted
        { invReqStart    :: !Integer
        , invReqReceived :: !Integer
        , invReqSent     :: !Integer
        , invReqClosed   :: !Integer
        }
    | InvReqRejected
        { invReqStart    :: !Integer
        , invReqReceived :: !Integer
        }
    | InvReqException !Text
    deriving Show

$(deriveJSON defaultOptions ''InvReqDataFlowLog)

invReqDataFlowNeighborsTK
    :: forall key contents m.
       ( Message (InvOrData (Tagged contents key) contents)
       , Message (ReqMsg (Tagged contents key))
       , Buildable key
       , Typeable contents
       , MinRelayWorkMode m
       , MonadGState m
       , MonadDiscovery m
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqMsg (Tagged contents key))
       )
    => Text -> SendActions m -> MsgType -> key -> contents -> m ()
invReqDataFlowNeighborsTK what sendActions msgType key dt =
    invReqDataFlowNeighbors what sendActions msgType key' dt
  where
    contProxy = (const Proxy :: contents -> Proxy contents) dt
    key' = tagWith contProxy key

invReqDataFlowTK
    :: forall key contents m.
       ( Message (InvOrData (Tagged contents key) contents)
       , Message (ReqMsg (Tagged contents key))
       , Buildable key
       , Typeable contents
       , MinRelayWorkMode m
       , MonadGState m
       , Bi (InvOrData (Tagged contents key) contents)
       , Bi (ReqMsg (Tagged contents key))
       )
    => Text -> SendActions m -> MsgType -> NodeId -> key -> contents -> m ()
invReqDataFlowTK what sendActions msgType addr key dt =
    invReqDataFlow what sendActions msgType addr key' dt
  where
    contProxy = (const Proxy :: contents -> Proxy contents) dt
    key' = tagWith contProxy key

invReqDataFlowNeighbors
    :: forall key contents m.
       ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGState m
       , MonadDiscovery m
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       )
    => Text -> SendActions m -> MsgType -> key -> contents -> m ()
invReqDataFlowNeighbors what sendActions msgType key dt = handleAll handleE $
    void $ converseToNeighbors sendActions (sendMsg msgType) (pure . Conversation . invReqDataFlowDo what key dt) >>= waitForConversations
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", key = "%build%" to neighbors: "%shown) what key e

invReqDataFlow
    :: forall key contents m.
       ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGState m
       )
    => Text -> SendActions m -> MsgType -> NodeId -> key -> contents -> m ()
invReqDataFlow what sendActions msgType addr key dt = handleAll handleE $
    void $ enqueueConversation' sendActions (S.singleton addr) (sendMsg msgType) $
        \_ _ -> pure $ Conversation $ invReqDataFlowDo what key dt addr
  where
    handleE e = logWarning $
        sformat ("Error sending "%stext%", key = "%build%" to "%shown%": "%shown)
                what key addr e

invReqDataFlowDo
    :: ( Message (InvOrData key contents)
       , Message (ReqMsg key)
       , Bi (InvOrData key contents)
       , Bi (ReqMsg key)
       , Buildable key
       , MinRelayWorkMode m
       , MonadGState m
       )
    => Text
    -> key
    -> contents
    -> NodeId
    -> ConversationActions (InvOrData key contents) (ReqMsg key) m
    -> m ()
invReqDataFlowDo what key dt peer conv = do
    send conv $ Left $ InvMsg key
    recvLimited conv >>= maybe handleD replyWithData
  where
    -- TODO need to check we're asked for same key we have
    replyWithData (ReqMsg _) = send conv $ Right $ DataMsg dt
    handleD =
        logDebug $
        sformat ("InvReqDataFlow ("%stext%"): "%shown %" closed conversation on \
                 \Inv key = "%build)
                what peer key

dataFlow
    :: forall contents m.
       ( Message (DataMsg contents)
       , Bi (DataMsg contents)
       , Buildable contents
       , MinRelayWorkMode m
       , Message Void
       )
    => Text -> SendActions m -> MsgType -> NodeId -> contents -> m ()
dataFlow what sendActions msgType addr dt = handleAll handleE $
    void $ enqueueConversation' sendActions (S.singleton addr) (sendMsg msgType) $
        \_ _ -> pure $ Conversation $ \(conv :: ConversationActions (DataMsg contents) Void m) ->
            send conv $ DataMsg dt
  where
    handleE e =
        logWarning $
        sformat ("Error sending "%stext%", data = "%build%" to "%shown%": "%shown)
                what dt addr e
