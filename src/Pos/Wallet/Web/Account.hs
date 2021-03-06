-- | Helpers for Wallet Set, Wallet and Account.

module Pos.Wallet.Web.Account
       ( myRootAddresses
       , getAddrIdx
       , getSKByAddr
       , getSKByAccAddr
       , genSaveRootKey
       , genUniqueAccountId
       , genUniqueAccountAddress
       , deriveAccountSK
       , deriveAccountAddress
       , AccountMode
       , GenSeed (..)
       , AddrGenSeed
       ) where

import           Data.List                  (elemIndex)
import           Formatting                 (build, sformat, (%))
import           System.Random              (randomIO)
import           Universum

import           Pos.Core                   (Address (..), deriveLvl2KeyPair)
import           Pos.Crypto                 (EncryptedSecretKey, PassPhrase, isHardened)
import           Pos.Util                   (maybeThrow)
import           Pos.Util.BackupPhrase      (BackupPhrase, safeKeysFromPhrase)
import           Pos.Wallet.KeyStorage      (MonadKeys, addSecretKey, getSecretKeys)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CId, CWAddressMeta (..), Wal,
                                             addrMetaToAccount, addressToCId, encToCId)
import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AddressLookupMode (Ever), WebWalletModeDB,
                                             doesWAddressExist, getAccountMeta)

type AccountMode ctx m = (MonadKeys ctx m, WebWalletModeDB ctx m, MonadThrow m)

myRootAddresses :: MonadKeys ctx m => m [CId Wal]
myRootAddresses = encToCId <<$>> getSecretKeys

getAddrIdx :: AccountMode ctx m => CId Wal -> m Int
getAddrIdx addr = elemIndex addr <$> myRootAddresses >>= maybeThrow notFound
  where notFound =
          RequestError $ sformat ("No wallet set with address "%build%" found") addr

getSKByAddr
    :: AccountMode ctx m
    => CId Wal
    -> m EncryptedSecretKey
getSKByAddr addr = do
    msk <- find (\k -> encToCId k == addr) <$> getSecretKeys
    maybeThrow notFound msk
  where notFound =
          RequestError $ sformat ("No wallet set with address "%build%" found") addr

getSKByAccAddr
    :: AccountMode ctx m
    => PassPhrase
    -> CWAddressMeta
    -> m EncryptedSecretKey
getSKByAccAddr passphrase addrMeta@CWAddressMeta {..} = do
    (addr, accKey) <-
        deriveAccountSK passphrase (addrMetaToAccount addrMeta) cwamAccountIndex
    let accCAddr = addressToCId addr
    if accCAddr /= cwamId
             -- if you see this error, maybe you generated public key address with
             -- no hd wallet attribute (if so, address would be ~half shorter than
             -- others)
        then throwM . InternalError $ "Account is contradictory!"
        else return accKey

genSaveRootKey
    :: AccountMode ctx m
    => PassPhrase
    -> BackupPhrase
    -> m EncryptedSecretKey
genSaveRootKey passphrase ph = do
    sk <- either keyFromPhraseFailed (pure . fst) $
        safeKeysFromPhrase passphrase ph
    addSecretKey sk
    return sk
  where
    keyFromPhraseFailed msg =
        throwM . RequestError $ "Key creation from phrase failed: " <> msg

data GenSeed a
    = DeterminedSeed a
    | RandomSeed

type AddrGenSeed = GenSeed Word32   -- with derivation index

generateUnique
    :: (MonadIO m, MonadThrow m)
    => Text -> AddrGenSeed -> (Word32 -> m b) -> (Word32 -> b -> m Bool) -> m b
generateUnique desc RandomSeed generator isDuplicate = loop (100 :: Int)
  where
    loop 0 = throwM . RequestError $
             sformat (build%": generation of unique item seems too difficult, \
                      \you are approaching the limit") desc
    loop i = do
        rand  <- liftIO randomIO
        value <- generator rand
        bad   <- orM
            [ isDuplicate rand value
            , pure $ isHardened rand  -- using hardened keys only for now
            ]
        if bad
            then loop (i - 1)
            else return value
generateUnique desc (DeterminedSeed seed) generator notFit = do
    value <- generator (fromIntegral seed)
    whenM (notFit seed value) $
        throwM . InternalError $
        sformat (build%": this index is already taken")
        desc
    return value

genUniqueAccountId
    :: AccountMode ctx m
    => AddrGenSeed
    -> CId Wal
    -> m AccountId
genUniqueAccountId genSeed wsCAddr =
    generateUnique
        "account generation"
        genSeed
        (return . AccountId wsCAddr)
        notFit
  where
    notFit _idx addr = isJust <$> getAccountMeta addr

genUniqueAccountAddress
    :: AccountMode ctx m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CWAddressMeta
genUniqueAccountAddress genSeed passphrase wCAddr@AccountId{..} =
    generateUnique "address generation" genSeed mkAccount notFit
  where
    mkAccount cwamAccountIndex =
        deriveAccountAddress passphrase wCAddr cwamAccountIndex
    notFit _idx addr = doesWAddressExist Ever addr

deriveAccountSK
    :: AccountMode ctx m
    => PassPhrase
    -> AccountId
    -> Word32
    -> m (Address, EncryptedSecretKey)
deriveAccountSK passphrase AccountId{..} accIndex = do
    -- this function is used in conditions when several secret keys with same
    -- public key are stored, thus checking for passphrase here as well
    let niceSK k = encToCId k == aiWId
    key <- maybeThrow noKey . find niceSK =<< getSecretKeys
    maybeThrow badPass $
        deriveLvl2KeyPair passphrase key aiIndex accIndex
  where
    noKey   = RequestError "No secret key with such address found"
    badPass = RequestError "Passphrase doesn't match"

deriveAccountAddress
    :: AccountMode ctx m
    => PassPhrase
    -> AccountId
    -> Word32
    -> m CWAddressMeta
deriveAccountAddress passphrase accId@AccountId{..} cwamAccountIndex = do
    (addr, _) <- deriveAccountSK passphrase accId cwamAccountIndex
    let cwamWId         = aiWId
        cwamWalletIndex = aiIndex
        cwamId          = addressToCId addr
    return CWAddressMeta{..}
