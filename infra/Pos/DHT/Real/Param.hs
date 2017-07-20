{-# LANGUAGE ScopedTypeVariables #-}

module Pos.DHT.Real.Param
       ( KademliaParams (..)
       ) where

import           Data.Aeson          (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), (.!=))
import qualified Data.Aeson          as A
import           Pos.DHT.Model.Types (DHTKey)
import           Pos.Util.TimeWarp   (NetworkAddress)
import           Universum

-- | Parameters for the Kademlia DHT subsystem.
data KademliaParams = KademliaParams
    { kpNetworkAddress  :: !NetworkAddress
    , kpPeers           :: ![NetworkAddress] -- ^ Peers passed from CLI
    , kpKey             :: !(Maybe DHTKey)
    , kpExplicitInitial :: !Bool
    , kpDump            :: !FilePath         -- ^ Path to kademlia dump file
    , kpExternalAddress :: !NetworkAddress   -- ^ External address of node
    } deriving (Show)

instance FromJSON KademliaParams where
    parseJSON = A.withObject "KademliaParams" $ \obj -> do
        kpPeers_ :: [(Text, Word16)]
                           <- obj .: "peers"
        bindHost :: Text   <- obj .: "host"
        bindPort :: Word16 <- obj .: "port"
        kpKey              <- obj .:? "key"
        kpExplicitInitial  <- obj .:? "explicitInitial" .!= False
        kpDump             <- obj .: "dumpFile"
        let kpExternalAddress = (bindHost, bindPort)
            kpNetworkAddress  = (bindHost, bindPort)
            kpPeers :: [NetworkAddress]
            kpPeers = fmap (first encodeUtf8) kpPeers_
        return $ KademliaParams {..}
