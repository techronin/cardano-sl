{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       ) where

import           Universum

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Store                 as Store
import qualified Network.Transport.Internal as NT (decodeWord32, encodeWord32)

import           Node.Message.Class         (Serializable (..))
import           Node.Message.Decoder       (Decoder (..))

import           Pos.Binary.Class           (Bi (..), encode, label)

data BiP = BiP

instance  Bi t => Serializable BiP t where
    -- Length-prefix the store-encoded body. The length is assumed to fit into
    -- 32 bits.
    packMsg _ t = encoded
      where
        encodedBody = encode t
        encodedLength = NT.encodeWord32 (fromIntegral (BS.length encodedBody))
        encoded = LBS.fromStrict (BS.append encodedLength encodedBody)

    unpackMsg _ = storeDecoder (label "BiP t" get) BS.empty

-- @pva701: It's copy-pasted from TW (due to it absents in the release branch of TW)
-- so it must be removed, when this commit
-- https://github.com/serokell/time-warp-nt/commit/8093761c30956eb5088a70da0ef971abd42ea842
-- will be in the release branch
storeDecoder :: Store.Peek t -> BS.ByteString  -> Decoder t
storeDecoder peek bs = Partial $ \mbs -> case mbs of
    Nothing -> Fail BS.empty (fromIntegral (BS.length bs)) "Unexpected end of input (length prefix)"
    Just bs' ->
        let (front, back) = BS.splitAt 4 (BS.append bs bs')
        in  if BS.length front == 4
            then storeDecoderBody peek (NT.decodeWord32 front) [] (Just back)
            -- In this case, back is empty and front has length strictly less
            -- than 4, so we have to wait for more input.
            else storeDecoder peek front

storeDecoderBody
    :: Store.Peek t
    -> Word32
    -> [BS.ByteString]
    -> Maybe BS.ByteString
    -> Decoder t
storeDecoderBody peek !remaining !acc !mbs = case mbs of
    Nothing -> Fail BS.empty (fromIntegral (BS.length (accumulate acc))) "Unexpected end of input (body)"
    Just bs ->
        let (front, back) = BS.splitAt (fromIntegral remaining) bs
            taken = fromIntegral (BS.length front)
            acc' = front : acc
            remaining' = remaining - taken
        in  if taken < remaining
            then Partial $ storeDecoderBody peek remaining' acc'
            else let body = accumulate acc' in case Store.decodeWith peek body of
                Left ex -> Fail back (fromIntegral (BS.length body)) (Store.peekExMessage ex)
                Right t -> Done back (fromIntegral (BS.length body)) t
  where
    accumulate :: [BS.ByteString] -> BS.ByteString
    accumulate = BS.concat . reverse

