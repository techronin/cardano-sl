{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Universum

import qualified Cardano.Crypto.Wallet      as CC
import qualified Crypto.ECC.Edwards25519    as Ed25519
import           Crypto.Hash                (digestFromByteString)
import qualified Crypto.PVSS                as Pvss
import qualified Crypto.Sign.Ed25519        as EdStandard
import qualified Data.Binary                as Binary
import qualified Data.ByteArray             as ByteArray
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Functor.Contravariant (contramap)
import           Data.SafeCopy              (SafeCopy (..))
import qualified Data.Store                 as Store
import qualified Data.Store.TH              as Store
import           Formatting                 (int, sformat, stext, (%))

import           Pos.Binary.Class           (AsBinary (..), Bi (..), Size (..),
                                             StaticSize (..), getBytes, getCopyBi, label,
                                             labelP, labelS, putBytes, putCopyBi,
                                             putField, sizeOf)
import qualified Pos.Binary.Class           as Bi
import           Pos.Crypto.Hashing         (AbstractHash (..), HashAlgorithm,
                                             WithHash (..), hashDigestSize',
                                             reifyHashDigestSize, withHash)
import           Pos.Crypto.HD              (HDAddressPayload (..))
import           Pos.Crypto.RedeemSigning   (RedeemPublicKey (..), RedeemSecretKey (..),
                                             RedeemSignature (..))
import           Pos.Crypto.SafeSigning     (EncryptedSecretKey (..), PassPhrase)
import           Pos.Crypto.SecretSharing   (EncShare (..), Secret (..), SecretProof (..),
                                             SecretSharingExtra (..), Share (..),
                                             VssKeyPair (..), VssPublicKey (..))
import           Pos.Crypto.Signing         (ProxyCert (..), ProxySecretKey (..),
                                             ProxySignature (..), PublicKey (..),
                                             SecretKey (..), Signature (..), Signed (..))

instance Bi a => Bi (WithHash a) where
    size = contramap whData size
    put = put . whData
    get = withHash <$> get

instance Bi a => SafeCopy (WithHash a) where
    putCopy = putCopyBi
    getCopy = getCopyBi "WithHash"

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance HashAlgorithm algo => Bi (AbstractHash algo a) where
    size = ConstSize (hashDigestSize' @algo)
    put (AbstractHash digest) = labelP "AbstractHash" $
        reifyHashDigestSize @algo (\(Proxy :: Proxy n) ->
            let bs = ByteArray.convert digest :: BS.ByteString
            in put (StaticSize @n bs))
    get =
        reifyHashDigestSize @algo (\(Proxy :: Proxy n) -> do
            sbs <- get
            let bs = unStaticSize @n sbs :: BS.ByteString
            case digestFromByteString bs of
                -- It's impossible because 'get' will already fail if there
                -- weren't enough bytes available
                Nothing -> error "AbstractHash.peek: impossible"
                Just x  -> pure (AbstractHash x))

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

-- [CSL-1122] TODO: move elsewhere?
constantSizedBinaryToStoreGet :: Binary.Binary a => Int -> Store.Peek a
constantSizedBinaryToStoreGet bytes = do
    x <- getBytes bytes
    case Binary.decodeOrFail (BSL.fromStrict x) of
        Left (_, _, err) -> fail err
        Right (bs, _, res)
            | BSL.null bs -> pure res
            | otherwise   -> fail "unconsumed input"

-- [CSL-1122] TODO: more efficient 'put' and 'get' are possible if 'Store'
-- instances are added into pvss-haskell
#define BiPvss(T, PT, BYTES) \
  instance Bi T where {\
    size = ConstSize BYTES ;\
    put = labelP "T" . putBytes . BSL.toStrict . Binary.encode ;\
    get = label "T" $ constantSizedBinaryToStoreGet BYTES };\
  deriving instance Bi PT ;\

BiPvss (Pvss.PublicKey, VssPublicKey, 33)    -- yes it's 33 and not 32
BiPvss (Pvss.KeyPair, VssKeyPair, 65)        -- 32+33
BiPvss (Pvss.Secret, Secret, 33)
BiPvss (Pvss.DecryptedShare, Share, 101)     -- 4+33+64
BiPvss (Pvss.EncryptedShare, EncShare, 101)
BiPvss (Pvss.Proof, SecretProof, 64)

instance Store.Store Pvss.ExtraGen where
    size = ConstSize 33
    poke = labelP "Pvss.ExtraGen" . putBytes . BSL.toStrict . Binary.encode
    peek = label "Pvss.ExtraGen" $ constantSizedBinaryToStoreGet 33
instance Store.Store Pvss.Commitment where
    size = ConstSize 33
    poke = labelP "Pvss.Commitment" . putBytes . BSL.toStrict . Binary.encode
    peek = label "Pvss.Commitment" $ constantSizedBinaryToStoreGet 33

Store.makeStore ''SecretSharingExtra
instance Bi SecretSharingExtra where
    put = labelP "SecretSharingExtra" . Store.poke
    get = label "SecretSharingExtra" $ Store.peek
    size = Store.size

deriving instance Bi (AsBinary SecretSharingExtra)

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

#define BiMacro(B, BYTES) \
  instance Bi (AsBinary B) where {\
    size = ConstSize BYTES ;\
    put (AsBinary bs) = putBytes bs ;\
    get = label "B (BYTES bytes)" $ AsBinary <$> getBytes BYTES}; \

BiMacro(VssPublicKey, 33)
BiMacro(Secret, 33)
BiMacro(Share, 101) --4+33+64
BiMacro(EncShare, 101)
BiMacro(SecretProof, 64)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

secretKeyLength, publicKeyLength, signatureLength, chainCodeLength,
    encryptedKeyLength, passphraseLength :: Int
secretKeyLength = 32
publicKeyLength = 32
encryptedKeyLength = 128
signatureLength = 64
chainCodeLength = 32
passphraseLength = 32

putAssertLength :: Monad m => Text -> Int -> ByteString -> m ()
putAssertLength typeName expectedLength bs =
    when (BS.length bs /= expectedLength) $ error $
        sformat ("put@"%stext%": expected length "%int%", not "%int)
                typeName expectedLength (BS.length bs)

instance Bi Ed25519.PointCompressed where
    size = ConstSize publicKeyLength
    put (Ed25519.unPointCompressed -> k) = labelP "Ed25519.PointCompressed" $ do
        putAssertLength "PointCompressed" publicKeyLength k
        putBytes k
    get = label "Ed25519.PointCompressed" $
        Ed25519.pointCompressed <$> getBytes publicKeyLength

instance Bi Ed25519.Scalar where
    size = ConstSize secretKeyLength
    put (Ed25519.unScalar -> k) = labelP "Ed25519.Scalar" $ do
        putAssertLength "Scalar" secretKeyLength k
        putBytes k
    get = label "Ed25519.Scalar" $
        Ed25519.scalar <$> getBytes secretKeyLength

instance Bi Ed25519.Signature where
    size = ConstSize signatureLength
    put (Ed25519.Signature s) = labelP "Ed25519.Signature" $ do
        putAssertLength "Signature" signatureLength s
        putBytes s
    get = label "Ed25519.Signature" $
        Ed25519.Signature <$> getBytes signatureLength

instance Bi CC.ChainCode where
    size = ConstSize chainCodeLength
    put (CC.ChainCode c) = do
        putAssertLength "ChainCode" chainCodeLength c
        putBytes c
    get = label "CC.ChainCode" $
        CC.ChainCode <$> getBytes chainCodeLength

instance Bi CC.XPub where
    size = ConstSize (publicKeyLength + chainCodeLength)
    put (CC.unXPub -> kc) = labelP "CC.XPub" $ do
        putAssertLength "XPub" (publicKeyLength + chainCodeLength) kc
        putBytes kc
    get = label "CC.XPub" $
        getBytes (publicKeyLength + chainCodeLength) >>=
        either fail pure . CC.xpub

instance Bi CC.XPrv where
    size = ConstSize encryptedKeyLength
    put (CC.unXPrv -> kc) = labelP "CC.XPrv" $ do
        putAssertLength "XPrv" encryptedKeyLength kc
        putBytes kc
    get = label "CC.XPrv" $
        getBytes encryptedKeyLength >>=
        either fail pure . CC.xprv

instance Bi CC.XSignature where
    size = ConstSize signatureLength
    put (CC.unXSignature -> bs) = labelP "CC.XSignature" $ do
        putAssertLength "XSignature" signatureLength bs
        putBytes bs
    get = label "CC.XSignature" $
        getBytes signatureLength >>=
        either fail pure . CC.xsignature

deriving instance Bi (Signature a)
deriving instance Bi PublicKey
deriving instance Bi SecretKey

instance Bi EncryptedSecretKey where
    size = Bi.combineSize (eskPayload, eskHash)
    put (EncryptedSecretKey sk pph) = put sk >> put pph
    get = label "EncryptedSecretKey" $ liftM2 EncryptedSecretKey get get

instance Bi a => Bi (Signed a) where
    size = Bi.combineSize (signedValue, signedSig)
    put (Signed v s) = put (v,s)
    get = label "Signed" $ Signed <$> get <*> get

deriving instance Bi (ProxyCert w)

instance (Bi w) => Bi (ProxySecretKey w) where
    sizeNPut = labelS "ProxySecretKey" $
        putField pskOmega <>
        putField pskIssuerPk <>
        putField pskDelegatePk <>
        putField pskCert
    get = label "ProxySecretKey" $ liftM4 ProxySecretKey get get get get

instance (Bi w) => Bi (ProxySignature w a) where
    sizeNPut = labelS "ProxySignature" $ putField psigPsk <> putField psigSig
    get = label "ProxySignature" $ liftM2 ProxySignature get get

instance Bi PassPhrase where
    size = ConstSize passphraseLength
    put pp = labelP "PassPhrase" $ do
        -- currently passphrase may be 32-byte long, or empty (for
        -- unencrypted keys). The empty passphrase is serialized as 32
        -- zeroes.
        let bs = ByteArray.convert pp
            bl = BS.length bs
        if | bl == 0 -> putBytes (BS.replicate passphraseLength 0)
           | bl == passphraseLength -> putBytes bs
           | otherwise -> error $ sformat
                 ("put@PassPhrase: expected length 0 or "%int%", not "%int)
                 passphraseLength bl
    get = let norm x | x == BS.replicate passphraseLength 0 = mempty
                     | otherwise                            = x
          in  label "PassPhrase" $
              ByteArray.convert . norm <$>
              getBytes passphraseLength

-------------------------------------------------------------------------------
-- Hierarchical derivation
-------------------------------------------------------------------------------

instance Bi HDAddressPayload where
    size = sizeOf getHDAddressPayload
    put (HDAddressPayload payload) = labelP "HDAddressPayload" $ put payload
    get = label "HDAddressPayload" $ HDAddressPayload <$> get

-------------------------------------------------------------------------------
-- Standard Ed25519 instances for ADA redeem keys
-------------------------------------------------------------------------------

standardSecretKeyLength, standardPublicKeyLength, standardSignatureLength :: Int
standardSecretKeyLength = 64
standardPublicKeyLength = 32
standardSignatureLength = 64

instance Bi EdStandard.PublicKey where
    size = ConstSize standardPublicKeyLength
    put (EdStandard.PublicKey k) = labelP "EdStandard.PublicKey" $ do
        putAssertLength "PublicKey" standardPublicKeyLength k
        putBytes k
    get = label "EdStandard.PublicKey" $
        EdStandard.PublicKey <$> getBytes standardPublicKeyLength

instance Bi EdStandard.SecretKey where
    size = ConstSize standardSecretKeyLength
    put (EdStandard.SecretKey k) = labelP "EdStandard.SecretKey" $ do
        putAssertLength "SecretKey" standardSecretKeyLength k
        putBytes k
    get = label "EdStandard.SecretKey" $
        EdStandard.SecretKey <$> getBytes standardSecretKeyLength

instance Bi EdStandard.Signature where
    size = ConstSize standardSignatureLength
    put (EdStandard.Signature s) = labelP "EdStandard.Signature" $ do
        putAssertLength "Signature" standardSignatureLength s
        putBytes s
    get = label "EdStandard.Signature" $
        EdStandard.Signature <$> getBytes standardSignatureLength

deriving instance Bi RedeemPublicKey
deriving instance Bi RedeemSecretKey
deriving instance Bi (RedeemSignature a)
