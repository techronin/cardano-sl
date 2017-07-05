{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure Toss.

module Pos.Ssc.GodTossing.Toss.Pure
       ( PureToss (..)
       , MultiRichmenStake
       , MultiRichmenSet
       , runPureToss
       , runPureTossWithLogger
       , evalPureTossWithLogger
       , execPureTossWithLogger
       ) where

import           Universum

import           Control.Lens                   (at, sequenceAOf, (%=), (.=))
import           Control.Monad.RWS.Strict       (RWST, runRWST)
import qualified Crypto.Random                  as Rand
import qualified Data.HashMap.Strict            as HM
import           System.Wlog                    (CanLog, HasLoggerName (..), LogEvent,
                                                 NamedPureLogger (..), WithLogger,
                                                 launchNamedPureLog, runNamedPureLog)

import           Pos.Core                       (EpochIndex, crucialSlot)
import           Pos.Lrc.Types                  (RichmenSet, RichmenStake)
import           Pos.Ssc.GodTossing.Core        (deleteSignedCommitment,
                                                 insertSignedCommitment)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Toss.Class  (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Types       (GtGlobalState, gsCommitments, gsOpenings,
                                                 gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD

type MultiRichmenStake = HashMap EpochIndex RichmenStake
type MultiRichmenSet = HashMap EpochIndex RichmenSet

-- 'MonadPseudoRandom' is needed because some cryptographic algorithms
-- require randomness even though they are deterministic. Note that running
-- them with the same seed every time is insecure and must not be done.
newtype PureToss a = PureToss
    { getPureToss :: NamedPureLogger (
                     RWST MultiRichmenStake () GtGlobalState (
                     Rand.MonadPseudoRandom Rand.ChaChaDRG)) a
    } deriving (Functor, Applicative, Monad,
                CanLog, HasLoggerName, Rand.MonadRandom)

instance MonadTossRead PureToss where
    getCommitments = PureToss $ use gsCommitments
    getOpenings = PureToss $ use gsOpenings
    getShares = PureToss $ use gsShares
    getVssCertificates = VCD.certs <$> getVssCertData
    getVssCertData = PureToss $ use $ gsVssCertificates
    getStableCertificates epoch
        | epoch == 0 = pure genesisCertificates
        | otherwise =
            PureToss $
            VCD.certs . VCD.setLastKnownSlot (crucialSlot epoch) <$>
            use gsVssCertificates
    getRichmen epoch = PureToss $ asks (HM.lookup epoch)

instance MonadToss PureToss where
    putCommitment signedComm =
        PureToss $ gsCommitments %= insertSignedCommitment signedComm
    putOpening id op = PureToss $ gsOpenings . at id .= Just op
    putShares id sh = PureToss $ gsShares . at id .= Just sh
    putCertificate cert =
        PureToss $ gsVssCertificates %= VCD.insert cert
    delCommitment id =
        PureToss $ gsCommitments %= deleteSignedCommitment id
    delOpening id = PureToss $ gsOpenings . at id .= Nothing
    delShares id = PureToss $ gsShares . at id .= Nothing
    resetCO = PureToss $ do
        gsCommitments .= mempty
        gsOpenings .= mempty
    resetShares = PureToss $ gsShares .= mempty
    setEpochOrSlot eos = PureToss $ gsVssCertificates %= VCD.setLastKnownEoS eos

runPureToss
    :: Rand.MonadRandom m
    => MultiRichmenStake
    -> GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState, [LogEvent])
runPureToss richmenData gs (PureToss act) = do
    seed <- Rand.drgNew
    let ((res, events), newGS, ()) =
            fst . Rand.withDRG seed $           -- run MonadRandom
            (\a -> runRWST a richmenData gs) $  -- run RWST
            runNamedPureLog act                 -- run NamedPureLogger
    pure (res, newGS, events)

runPureTossWithLogger
    :: forall m a. (WithLogger m, Rand.MonadRandom m)
    => MultiRichmenStake
    -> GtGlobalState
    -> PureToss a
    -> m (a, GtGlobalState)
runPureTossWithLogger richmenData gs (PureToss act) = do
    seed <- Rand.drgNew
    let unwrapLower :: forall f. Functor f
                    => RWST MultiRichmenStake () GtGlobalState
                            (Rand.MonadPseudoRandom Rand.ChaChaDRG)
                            (f a)
                    -> m (f (a, GtGlobalState, ()))
        unwrapLower a = pure $ sequenceAOf _1 $     -- (f a,b,c) -> f (a,b,c)
                        fst $ Rand.withDRG seed $   -- run MonadRandom
                        runRWST a richmenData gs    -- run RWST
    (res, newGS, ()) <- launchNamedPureLog unwrapLower act
    return (res, newGS)

evalPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => MultiRichmenStake -> GtGlobalState -> PureToss a -> m a
evalPureTossWithLogger r g = fmap fst . runPureTossWithLogger r g

execPureTossWithLogger
    :: (WithLogger m, Rand.MonadRandom m)
    => MultiRichmenStake -> GtGlobalState -> PureToss a -> m GtGlobalState
execPureTossWithLogger r g = fmap snd . runPureTossWithLogger r g
