{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | AsBinary wrappers for Pos.Crypto.SecretSharing types.

module Pos.Crypto.AsBinary () where

import           Universum

import qualified Data.ByteString          as BS
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))

import           Pos.Binary.Class         (AsBinary (..), AsBinaryClass (..), Bi,
                                           decodeFull, encode)
import           Pos.Binary.Size          (ExactSized, exactSize')
import           Pos.Crypto.Hashing       (hash, shortHashF)
import           Pos.Crypto.SecretSharing (DecShare (..), EncShare (..), Secret (..),
                                           SecretProof (..), SecretProof (..),
                                           VssPublicKey (..))

----------------------------------------------------------------------------
-- AsBinary type wrappers
--
-- Wrappers over ByteString to allow transmitting crypto data types
-- over network without high costs on serialization/hashing
----------------------------------------------------------------------------

checkLen :: forall a. ExactSized a => Text -> Text -> ByteString -> ByteString
checkLen action name bs =
    maybe bs error $
    checkLenImpl action name (exactSize' @a) (BS.length bs)

checkLenImpl :: Text -> Text -> Int -> Int -> Maybe Text
checkLenImpl action name expectedLen len
    | expectedLen == len = Nothing
    | otherwise =
        Just $
        sformat
            (stext % " " %stext % " failed: length of bytestring is " %int %
             " instead of " %int)
            action
            name
            len
            expectedLen

#define Ser(B) \
  instance (Bi B, Bi (AsBinary B), ExactSized B) => AsBinaryClass B where {\
    asBinary = AsBinary . checkLen @B "asBinary" "B" . encode ;\
    fromBinary = decodeFull . checkLen @B "fromBinary" "B" . encode }; \

Ser(VssPublicKey)
Ser(Secret)
Ser(DecShare)
Ser(EncShare)

instance (Bi SecretProof, Bi (AsBinary SecretProof)) =>
         AsBinaryClass SecretProof where
    asBinary = AsBinary . encode
    fromBinary = decodeFull . encode

instance Buildable (AsBinary Secret) where
    build _ = "secret \\_(o.o)_/"

instance Buildable (AsBinary DecShare) where
    build _ = "share \\_(*.*)_/"

instance Buildable (AsBinary EncShare) where
    build _ = "encrypted share \\_(0.0)_/"

instance Bi (AsBinary VssPublicKey) => Buildable (AsBinary VssPublicKey) where
    build = bprint ("vsspub:"%shortHashF) . hash
