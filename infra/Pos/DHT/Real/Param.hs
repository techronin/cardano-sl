module Pos.DHT.Real.Param
       ( KademliaParams(..)
       ) where

import           Pos.DHT.Model.Types (DHTKey, DHTNode)
import           Pos.Util.TimeWarp   (NetworkAddress)
import           Universum

-- | Parameters for the Kademlia DHT subsystem.
data KademliaParams = KademliaParams
    { kpNetworkAddress  :: !NetworkAddress
    , kpPeers           :: ![DHTNode]      -- ^ Peers passed from CLI
    , kpKey             :: !(Maybe DHTKey)
    , kpExplicitInitial :: !Bool
    , kpDump            :: !FilePath       -- ^ Path to kademlia dump file
    , kpBehindNat       :: !Bool           -- ^ Run our node behind NAT
    }
    deriving (Show)
