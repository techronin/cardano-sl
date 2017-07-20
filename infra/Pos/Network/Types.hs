module Pos.Network.Types
    ( NetworkConfig (..)
    , Topology(..)
    , topologyNodeType
      -- * Re-exports
      -- ** from .Yaml
    , DnsDomains(..)
      -- ** from time-warp
    , NodeType (..)
    , MsgType (..)
    , Origin (..)
      -- ** other
    , NodeId (..)
    ) where

import           Universum
import           Network.Broadcast.OutboundQueue.Types
import           Node.Internal (NodeId (..))
import           Pos.Network.Yaml (DnsDomains (..))
import           Pos.DHT.Real.Param (KademliaParams (..))

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
  | TopologyP2P !KademliaParams

    -- | We discover our peers through Kademlia, and every node in the network
    -- is a core node.
    --
    -- TODO: This is temporary.
  | TopologyTransitional !KademliaParams
  deriving (Show)

-- | Derive node type from its topology
topologyNodeType :: Topology -> NodeType
topologyNodeType (TopologyStatic nodeType _) = nodeType
topologyNodeType (TopologyBehindNAT _)       = NodeEdge
topologyNodeType (TopologyP2P _)             = NodeEdge
topologyNodeType (TopologyTransitional _)    = NodeCore
