-- | Infrastructure for parsing the .yaml network topology file
module Pos.Network.Yaml (
    Topology(..)
  , AllStaticallyKnownPeers(..)
  , DnsDomains(..)
  , NodeName(..)
  , NodeRegion(..)
  , NodeRoutes(..)
  , NodeMetadata(..)
  ) where

import           Universum
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import           Network.Broadcast.OutboundQueue.Types
import           Pos.Util.Config
import qualified Data.Aeson            as A
import qualified Data.Aeson.Types      as A
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.HashMap.Lazy     as HM
import qualified Data.Map.Strict       as M
import qualified Network.DNS           as DNS

-- | Description of the network topology in a Yaml file
--
-- This differs from 'Pos.Network.Types.Topology' because for static nodes this
-- describes the entire network topology (all statically known nodes), not just
-- the topology from the point of view of the current node.
data Topology =
    TopologyStatic !AllStaticallyKnownPeers
  | TopologyBehindNAT !DnsDomains
  | TopologyP2P !KademliaParams
  | TopologyTransitional !KademliaParams
  deriving (Show)

-- | All statically known peers in the newtork
data AllStaticallyKnownPeers = AllStaticallyKnownPeers {
    allStaticallyKnownPeers :: !(Map NodeName NodeMetadata)
  }
  deriving (Show)

-- | DNS domains for relay discovery
--
-- We provide a list of list of domain names to query. The outer list
-- corresponds to backup DNS servers; the inner lists provide multiple
-- domain names which serve different purposes (e.g., the first might be
-- configured to return geolocated hosts, with the second a load-balancing
-- fall-back). The idea is that querying each of the elements of the inner
-- lists provides a complete set of relay nodes to be tried, using the
-- backup domain names only when one or more of the primary ones fail.
data DnsDomains = DnsDomains {
      dnsDomains :: !(Alts (AllOf DNS.Domain))
    }
  deriving (Show)

newtype NodeName = NodeName Text
    deriving (Show, Ord, Eq, IsString)

newtype NodeRegion = NodeRegion Text
    deriving (Show, Ord, Eq, IsString)

newtype NodeRoutes = NodeRoutes [[NodeName]]
    deriving (Show)

data NodeMetadata = NodeMetadata
    { nmType   :: !NodeType
    , nmRegion :: !NodeRegion
    , nmRoutes :: !NodeRoutes
    }
    deriving (Show)

-- | Parameters for Kademlia, in case P2P or transitional topology are used.
data KademliaParams = KademliaParams
    { kpId       :: !(Maybe KademliaId)
      -- ^ Kademlia identifier. Optional; one can be generated for you.
    , kpPeer     :: !KademliaAddress
      -- ^ Initial Kademlia peer, for joining the network.
    , kpAddress  :: !(Maybe KademliaAddress)
      -- ^ External Kadmelia address.
    , kpBind     :: !(Maybe KademliaAddress)
      -- ^ Address at which to bind the Kademlia socket.
    , kpDumpFile :: !(Maybe FilePath)
    }
    deriving (Show)

-- | A Kademlia identifier in text representation (probably base64-url encoded).
newtype KademliaId = KademliaId Text
    deriving (Show)

data KademliaAddress = KademliaAddress
    { kaHost :: !String
    , kaPort :: !Word16
    }
    deriving (Show)

{-------------------------------------------------------------------------------
  FromJSON instances
-------------------------------------------------------------------------------}

instance FromJSON NodeRegion where
  parseJSON = fmap NodeRegion . parseJSON

instance FromJSON NodeName where
  parseJSON = fmap NodeName . parseJSON

instance FromJSON NodeRoutes where
  parseJSON = fmap NodeRoutes . parseJSON

instance FromJSON NodeType where
  parseJSON = A.withText "NodeType" $ \typ -> do
      case toString typ of
        "core"     -> return NodeCore
        "edge"     -> return NodeEdge
        "relay"    -> return NodeRelay
        _otherwise -> fail $ "Invalid NodeType " ++ show typ

instance FromJSON DnsDomains where
  parseJSON = fmap (DnsDomains . aux) . parseJSON
    where
      aux :: [[String]] -> [[DNS.Domain]]
      aux = map (map BS.C8.pack)

instance FromJSON NodeMetadata where
  parseJSON = A.withObject "NodeMetadata" $ \obj -> do
      nmType   <- obj .: "type"
      nmRegion <- obj .: "region"
      nmRoutes <- obj .: "static-routes"
      return NodeMetadata{..}

instance FromJSON AllStaticallyKnownPeers where
  parseJSON = A.withObject "AllStaticallyKnownPeers" $ \obj ->
      AllStaticallyKnownPeers . M.fromList <$> mapM aux (HM.toList obj)
    where
      aux :: (Text, A.Value) -> A.Parser (NodeName, NodeMetadata)
      aux (name, val) = (NodeName name, ) <$> parseJSON val

instance FromJSON Topology where
  parseJSON = A.withObject "Topology" $ \obj -> do
      mNodes  <- obj .:? "nodes"
      mRelays <- obj .:? "relays"
      mP2p    <- obj .:? "p2p"
      mTrans  <- obj .:? "transitional"
      case (mNodes, mRelays, mP2p, mTrans) of
        (Just nodes, Nothing, Nothing, Nothing) ->
            TopologyStatic <$> parseJSON nodes
        (Nothing, Just relays, Nothing, Nothing) ->
            TopologyBehindNAT <$> parseJSON relays
        (Nothing, Nothing, Just p2p, Nothing) ->
            TopologyP2P <$> parseJSON p2p
        (Nothing, Nothing, Nothing, Just transitional) ->
            TopologyTransitional <$> parseJSON transitional
        _ ->
          fail "Topology: expected exactly one of 'nodes', 'relays', 'p2p', 'transitional'"

instance IsConfig Topology where
  configPrefix = return Nothing

{-------------------------------------------------------------------------------
  ToJSON instances
-------------------------------------------------------------------------------}

instance ToJSON NodeRegion where
  toJSON (NodeRegion region) = toJSON region

instance ToJSON NodeName where
  toJSON (NodeName name) = toJSON name

instance ToJSON NodeRoutes where
  toJSON (NodeRoutes routes) = toJSON routes

instance ToJSON NodeType where
  toJSON NodeCore  = toJSON ("core"  :: Text)
  toJSON NodeEdge  = toJSON ("edge"  :: Text)
  toJSON NodeRelay = toJSON ("relay" :: Text)

instance ToJSON DnsDomains where
  toJSON DnsDomains{..} = toJSON $ aux dnsDomains
    where
      aux :: [[DNS.Domain]] -> [[String]]
      aux = map (map BS.C8.unpack)

instance ToJSON NodeMetadata where
  toJSON NodeMetadata{..} = A.object [
        "type"          .= nmType
      , "region"        .= nmRegion
      , "static-routes" .= nmRoutes
      ]

instance ToJSON AllStaticallyKnownPeers where
  toJSON AllStaticallyKnownPeers{..} =
      A.object (map aux $ M.toList allStaticallyKnownPeers)
    where
      aux :: (NodeName, NodeMetadata) -> A.Pair
      aux (NodeName name, info) = name .= info

instance ToJSON Topology where
  toJSON (TopologyStatic    nodes)  = A.object [ "nodes"        .= nodes  ]
  toJSON (TopologyBehindNAT relays) = A.object [ "relays"       .= relays ]
  toJSON (TopologyP2P kp)           = A.object [ "p2p"          .= kp     ]
  toJSON (TopologyTransitional kp)  = A.object [ "transitional" .= kp     ]
