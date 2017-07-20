-- | Getter params from Args

module Params
       ( loggingParams
       , getNodeParams
       , gtSscParams
       ) where

import           Universum

import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Data.Set              as S (fromList)
import qualified Network.Transport.TCP as TCP (TCPAddr (..), TCPAddrInfo (..))
import           System.Wlog           (LoggerName, WithLogger)

import qualified Pos.CLI               as CLI
import           Pos.Constants         (isDevelopment)
import           Pos.Communication     (NodeId)
import           Pos.Core.Types        (Timestamp (..))
import           Pos.Crypto            (VssKeyPair)
import           Pos.DHT.Real          (KademliaParams (..))
import           Pos.Genesis           (devAddrDistr, devStakesDistr,
                                        genesisProdAddrDistribution,
                                        genesisProdBootStakeholders, genesisUtxo)
import           Pos.Launcher          (BaseParams (..), LoggingParams (..),
                                        NetworkParams (..), NodeParams (..))
import           Pos.Network.CLI       (intNetworkConfigOpts)
import           Pos.Security          (SecurityParams (..))
import           Pos.Ssc.GodTossing    (GtParams (..))
import           Pos.Update.Params     (UpdateParams (..))
import           Pos.Util.UserSecret   (peekUserSecret)

import           NodeOptions           (Args (..))
import           Secrets               (updateUserSecretVSS, userSecretWithGenesisKey)


loggingParams :: LoggerName -> Args -> LoggingParams
loggingParams tag Args{..} =
    LoggingParams
    { lpHandlerPrefix = CLI.logPrefix commonArgs
    , lpConfigPath    = CLI.logConfig commonArgs
    , lpRunnerTag     = tag
    }

getPeersFromArgs :: Args -> [NodeId]
getPeersFromArgs Args {..} = fst <$> peers
    -- FIXME change the format of this file? Eliminate it altogether and
    -- consolidate this configuration information in one file, with other
    -- networking/relay policy stuff?
    --filePeers <- maybe (return []) readAddrFile peersFile
    --pure $ peers ++ filePeers

-- | Load up the KademliaParams. It's in IO because we may have to read a
--   file to find some peers.
getKademliaParams :: Args -> KademliaParams
getKademliaParams Args{..} =
    KademliaParams
        { kpNetworkAddress  = dhtNetworkAddress
        , kpPeers           = dhtPeers
        , kpKey             = dhtKey
        , kpExplicitInitial = dhtExplicitInitial
        , kpDump            = kademliaDumpPath
        , kpExternalAddress = externalAddress
        }

getBaseParams :: LoggerName -> Args -> BaseParams
getBaseParams loggingTag args@Args {..} =
    BaseParams { bpLoggingParams = loggingParams loggingTag args }

gtSscParams :: Args -> VssKeyPair -> GtParams
gtSscParams Args {..} vssSK =
    GtParams
    { gtpSscEnabled = True
    , gtpVssKeyPair = vssSK
    }

getKeyfilePath :: Args -> FilePath
getKeyfilePath Args {..}
    | isDevelopment = case devSpendingGenesisI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath
    | otherwise = keyfilePath

getNodeParams ::
       (MonadIO m, MonadFail m, MonadThrow m, WithLogger m)
    => Args
    -> Timestamp
    -> m NodeParams
getNodeParams args@Args {..} systemStart = do
    (primarySK, userSecret) <-
        userSecretWithGenesisKey args =<<
        updateUserSecretVSS args =<<
        peekUserSecret (getKeyfilePath args)
    npNetworkConfig <- liftIO $ intNetworkConfigOpts networkConfigOpts
    let npNetwork = getNetworkParams args
        devStakeDistr =
            devStakesDistr
                (CLI.flatDistr commonArgs)
                (CLI.bitcoinDistr commonArgs)
                (CLI.richPoorDistr commonArgs)
                (CLI.expDistr commonArgs)
        npCustomUtxo =
            if isDevelopment
            then genesisUtxo Nothing (devAddrDistr devStakeDistr)
            else genesisUtxo (Just genesisProdBootStakeholders)
                             genesisProdAddrDistribution
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npSystemStart = systemStart
        , npBaseParams = getBaseParams "node" args
        , npJLFile = jlPath
        , npPropagation = not (CLI.disablePropagation commonArgs)
        , npReportServers = CLI.reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = CLI.updateServers commonArgs
            }
        , npSecurityParams = SecurityParams
            { spAttackTypes   = maliciousEmulationAttacks
            , spAttackTargets = maliciousEmulationTargets
            }
        , npUseNTP = not noNTP
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }

getNetworkParams :: Args -> NetworkParams
getNetworkParams args
    | staticPeers args =
        let allPeers = S.fromList (getPeersFromArgs args)
        in  NetworkParams {npDiscovery = Left allPeers, npTcpAddr = TCP.Unaddressable}
    | otherwise =
        let (bindHost, bindPort) = bindAddress args
            (externalHost, externalPort) = externalAddress args
            tcpAddr =
                TCP.Addressable $
                TCP.TCPAddrInfo
                    (BS8.unpack bindHost)
                    (show $ bindPort)
                    (const (BS8.unpack externalHost, show $ externalPort))
            kademliaParams = getKademliaParams args
        in  NetworkParams {npDiscovery = Right kademliaParams, npTcpAddr = tcpAddr}
