-- | This module tests Binary instances for Pos.Communication types

module Test.Pos.Communication.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec               (Spec, describe)

import qualified Pos.Communication        as C
import           Pos.Infra.Arbitrary      ()

import           Test.Pos.Util            (networkBinaryTest)

spec :: Spec
spec = describe "Communication" $ do
    describe "Bi instances" $ do
        networkBinaryTest @C.HandlerSpec
        networkBinaryTest @C.VerInfo
