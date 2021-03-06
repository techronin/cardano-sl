-- | Arbitrary instances for Delegation types.

module Pos.Delegation.Arbitrary
       ( genDlgPayload
       ) where

import           Universum

import qualified Data.HashMap.Strict               as HM
import           Test.QuickCheck                   (Arbitrary (..), Gen, listOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Binary.Core                   ()
import           Pos.Communication.Types.Relay     (DataMsg (..))
import           Pos.Core                          (EpochIndex)
import           Pos.Core.Arbitrary                ()
import           Pos.Crypto                        (ProxySecretKey (..), createPsk)
import           Pos.Delegation.Types              (DlgPayload, mkDlgPayload,
                                                    ProxySKLightConfirmation)
import           Pos.Util.Util                     (leftToPanic)

genDlgPayload :: EpochIndex -> Gen DlgPayload
genDlgPayload epoch =
    leftToPanic "genDlgPayload: " .
    mkDlgPayload . toList . HM.fromList . map convert <$>
    listOf genPSK
  where
    convert psk = (pskIssuerPk psk, psk)
    genPSK = createPsk <$> arbitrary <*> arbitrary <*> pure epoch

instance Arbitrary DlgPayload where
    arbitrary = arbitrary >>= genDlgPayload
    shrink = genericShrink

instance Arbitrary (DataMsg ProxySKLightConfirmation) where
    arbitrary = genericArbitrary
    shrink = genericShrink
