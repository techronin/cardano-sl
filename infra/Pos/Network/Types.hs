module Pos.Network.Types
    ( NetworkConfig (..)
    , Topology(..)
    , topologyNodeType
    , resolveDnsDomains
      -- * Re-exports
      -- ** from .DnsDomains
    , DnsDomains(..)
    , DNSError
      -- ** from time-warp
    , NodeType (..)
    , MsgType (..)
    , Origin (..)
      -- ** other
    , NodeId (..)
    ) where

import           Universum
import           Data.IP (IPv4)
import           Network.Broadcast.OutboundQueue.Types
import           Node.Internal (NodeId (..))
import           Pos.Network.DnsDomains (DnsDomains(..), DNSError)
import           Pos.Util.TimeWarp  (addressToNodeId)
import qualified Pos.Network.DnsDomains as DnsDomains
import qualified Data.ByteString.Char8  as BS.C8

-- | Information about the network in which a node participates.
data NetworkConfig = NetworkConfig
    { ncTopology :: !Topology
      -- ^ Network topology from the point of view of the current node
    , ncDefaultPort :: !Word16
      -- ^ Port number to use when translating IP addresses to NodeIds
    }
  deriving (Show)

-- | Topology of the network, from the point of view of the current node
data Topology =
    -- | All peers of the node have been statically configured
    --
    -- This is used for core and relay nodes
    TopologyStatic !NodeType !(Peers NodeId)

    -- | We discover our peers through DNS
    --
    -- This is used for behind-NAT nodes.
  | TopologyBehindNAT !DnsDomains

    -- | We discover our peers through Kademlia
    --
    -- This is used for exchanges.
    --
    -- TODO: Not sure what parameters we need here; possibly the
    -- 'NetworkParams' type from 'Pos.Launcher.Param'
  | TopologyP2P

    -- | We discover our peers through Kademlia, and every node in the network
    -- is a core node.
    --
    -- TODO: Not sure what parameters we need here; see 'TopologyP2P'.
    -- TODO: This is temporary.
  | TopologyTransitional
  deriving (Show)

-- | Derive node type from its topology
topologyNodeType :: Topology -> NodeType
topologyNodeType (TopologyStatic nodeType _) = nodeType
topologyNodeType (TopologyBehindNAT _)       = NodeEdge
topologyNodeType (TopologyP2P)               = NodeEdge
topologyNodeType (TopologyTransitional)      = NodeCore

-- | Variation on resolveDnsDomains that returns node IDs
resolveDnsDomains :: NetworkConfig
                  -> DnsDomains
                  -> IO (Either [DNSError] [NodeId])
resolveDnsDomains NetworkConfig{..} dnsDomains =
    fmap (map ipv4ToNodeId) <$> DnsDomains.resolveDnsDomains dnsDomains
  where
    -- | Turn IPv4 address returned by DNS into a NodeId
    ipv4ToNodeId :: IPv4 -> NodeId
    ipv4ToNodeId addr = addressToNodeId (BS.C8.pack (show addr), ncDefaultPort)
