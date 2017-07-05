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
import qualified Crypto.SCRAPE              as Scrape
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
                                             StaticSize (..), getBytes, getCopyBi,
                                             getSize, label, labelP, labelS, putBytes,
                                             putCopyBi, putField, sizeOf)
import qualified Pos.Binary.Class           as Bi
import           Pos.Binary.Size            (ExactSize (..), ExactSized, exactSize,
                                             exactSize')
import           Pos.Crypto.Hashing         (AbstractHash (..), HashAlgorithm,
                                             WithHash (..), hashDigestSize',
                                             reifyHashDigestSize, withHash)
import           Pos.Crypto.HD              (HDAddressPayload (..))
import           Pos.Crypto.RedeemSigning   (RedeemPublicKey (..), RedeemSecretKey (..),
                                             RedeemSignature (..))
import           Pos.Crypto.SafeSigning     (EncryptedSecretKey (..), PassPhrase)
import qualified Pos.Crypto.SecretSharing   as C
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

instance HashAlgorithm algo => ExactSized (AbstractHash algo a) where
    exactSize = ExactSize (hashDigestSize' @algo)

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
#define StorePvss(T) \
  instance Store.Store T where {\
    size = ConstSize (exactSize' @T) ;\
    poke = labelP "T" . putBytes . BSL.toStrict . Binary.encode ;\
    peek = label "T" $ constantSizedBinaryToStoreGet (exactSize' @T) };\

#define BiPvss(T, PT) \
  StorePvss(T) \
  instance Bi T where { \
    size = Store.size ;\
    put  = Store.poke ;\
    get  = Store.peek };\
  deriving instance Bi PT ;\
  instance ExactSized PT where exactSize = PT <$> exactSize ;\

-- These instances have to be believed
instance ExactSized Scrape.Point where
    exactSize = 33       -- not 32!
instance ExactSized Scrape.Scalar where
    exactSize = 32
instance ExactSized Scrape.Proof where
    exactSize = ExactSize $
        32 {- exactSize @Challenge -} +
        exactSize' @Scrape.Scalar

-- This instances are correct by construction
instance ExactSized Pvss.PublicKey where
    exactSize = Pvss.PublicKey <$> exactSize
instance ExactSized Pvss.PrivateKey where
    exactSize = Pvss.PrivateKey <$> exactSize
instance ExactSized Pvss.KeyPair where
    exactSize = Pvss.KeyPair <$> exactSize <*> exactSize
instance ExactSized Scrape.DecryptedShare where
    exactSize = Scrape.DecryptedShare <$> exactSize <*> exactSize
instance ExactSized Scrape.EncryptedSi where
    exactSize = Scrape.EncryptedSi <$> exactSize
instance ExactSized Scrape.Secret where
    exactSize = Scrape.Secret <$> exactSize
instance ExactSized Scrape.ExtraGen where
    exactSize = Scrape.ExtraGen <$> exactSize
instance ExactSized Scrape.Commitment where
    exactSize = Scrape.Commitment <$> exactSize

BiPvss (Pvss.PublicKey, C.VssPublicKey)
BiPvss (Pvss.KeyPair, C.VssKeyPair)
BiPvss (Scrape.Secret, C.Secret)
BiPvss (Scrape.DecryptedShare, C.DecShare)
BiPvss (Scrape.EncryptedSi, C.EncShare)

StorePvss(Scrape.Proof)
StorePvss(Scrape.ExtraGen)
StorePvss(Scrape.Commitment)

-- TODO: less optimal than it could be (because it writes the length even
-- though technically it doesn't have to)
instance Store.Store Scrape.ParallelProofs where
  size = VarSize (getSize . Binary.encode)
  poke = labelP "ParallelProofs" . put . Binary.encode
  peek = label "ParallelProofs" $ do
      bs <- get
      case Binary.decodeOrFail bs of
          Left (_, _, s)   -> fail s
          Right ("", _, a) -> pure a
          Right (x, _, _)  -> fail (show (BSL.length x) <> " unconsumed bytes")

Store.makeStore ''C.SecretProof
instance Bi C.SecretProof where
    put = labelP "SecretProof" . Store.poke
    get = label "SecretProof" $ Store.peek
    size = Store.size

deriving instance Bi (AsBinary C.SecretProof)

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

#define BiMacro(B) \
  instance Bi (AsBinary B) where {\
    size = ConstSize (exactSize' @B) ;\
    put (AsBinary bs) = putBytes bs ;\
    get = label "B" $ \
              AsBinary <$> getBytes (exactSize' @B)}; \

BiMacro(C.VssPublicKey)
BiMacro(C.Secret)
BiMacro(C.DecShare)
BiMacro(C.EncShare)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

secretKeyLength, publicKeyLength, signatureLength, chainCodeLength,
    encryptedKeyLength, passphraseLength :: Int
secretKeyLength = 32
publicKeyLength = 32
encryptedKeyLength = 96
signatureLength = 64
chainCodeLength = 32
passphraseLength = 32

instance ExactSized Ed25519.PointCompressed where
    exactSize = ExactSize publicKeyLength
instance ExactSized Ed25519.Scalar where
    exactSize = ExactSize secretKeyLength
instance ExactSized Ed25519.Signature where
    exactSize = ExactSize signatureLength

instance ExactSized CC.ChainCode where
    exactSize = ExactSize chainCodeLength
instance ExactSized CC.XPub where
    exactSize = ExactSize (publicKeyLength + chainCodeLength)
instance ExactSized CC.XPrv where
    exactSize = ExactSize encryptedKeyLength
instance ExactSized CC.XSignature where
    exactSize = ExactSize signatureLength

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

deriving instance ExactSized (Signature a)
deriving instance ExactSized PublicKey
deriving instance ExactSized SecretKey

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
