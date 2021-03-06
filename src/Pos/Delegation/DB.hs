{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for heavyweight
-- delegation.
--
-- It stores three mappings:
--
-- 1. Psk mapping: Issuer → PSK, where pskIssuer of PSK is Issuer (must
-- be consistent). We don't store revocation psks, instead we just
-- delete previous Issuer → PSK. DB must not contain revocation psks.
--
-- 2. Dlg transitive mapping: Issuer → Delegate. This one is
-- transitive relation "i delegated to d through some chain of
-- certificates". DB must not contain cycles in psk mapping. As
-- mappings of kind I → I are forbidden (no revocation psks), Delegate
-- is always different from Issuer.
--
-- 3. Dlg reverse transitive mapping: Delegate →
-- Issuers@{Issuer_i}. Corresponds to Issuer → Delegate ∈ Dlg transitive
-- mapping. Notice: here also Delegate ∉ Issuers (see (2)).

module Pos.Delegation.DB
       (
         -- * Getters and predicates
         getPskByIssuer
       , isIssuerByAddressHash
       , getDlgTransitive
       , getDlgTransitiveReverse

         -- * Batch ops
       , DelegationOp (..)

         -- * Iteration
       , DlgTransRevIter
       , getDelegators
       ) where

import           Universum

import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (Source)
import qualified Data.HashSet                 as HS
import qualified Database.RocksDB             as Rocks

import           Pos.Binary.Class             (encode)
import           Pos.Crypto                   (PublicKey, pskIssuerPk, verifyPsk)
import           Pos.DB                       (RocksBatchOp (..), encodeWithKeyPrefix)
import           Pos.DB.Class                 (DBIteratorClass (..), DBTag (..),
                                               MonadDBRead (..))
import           Pos.DB.GState.Common         (gsGetBi)
import           Pos.Delegation.Cede.Types    (DlgEdgeAction (..))
import           Pos.Delegation.Helpers       (isRevokePsk)
import           Pos.Types                    (ProxySKHeavy, StakeholderId, addressHash)

----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer public key or his
-- address/stakeholder id, if present.
getPskByIssuer
    :: MonadDBRead m
    => Either PublicKey StakeholderId -> m (Maybe ProxySKHeavy)
getPskByIssuer (either addressHash identity -> issuer) = gsGetBi (pskKey issuer)

-- | Checks if stakeholder is psk issuer.
isIssuerByAddressHash :: MonadDBRead m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPskByIssuer . Right

-- | Given issuer @i@ returns @d@ such that there exists @x1..xn@ with
-- @i→x1→...→xn→d@.
getDlgTransitive :: MonadDBRead m => StakeholderId -> m (Maybe StakeholderId)
getDlgTransitive issuer = gsGetBi (transDlgKey issuer)

-- | Reverse map of transitive delegation. Given a delegate @d@
-- returns all @i@ such that 'getDlgTransitive' returns @d@ on @i@.
getDlgTransitiveReverse :: MonadDBRead m => StakeholderId -> m (HashSet StakeholderId)
getDlgTransitiveReverse dPk = fromMaybe mempty <$> gsGetBi (transRevDlgKey dPk)

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = PskFromEdgeAction !DlgEdgeAction
    -- ^ Adds or removes Psk. Overwrites on addition if present.
    | AddTransitiveDlg !StakeholderId !StakeholderId
    -- ^ Transitive delegation relation adding.
    | DelTransitiveDlg !StakeholderId
    -- ^ Remove i -> d link for i.
    | SetTransitiveDlgRev !StakeholderId !(HashSet StakeholderId)
    -- ^ Set value to map d -> [i], reverse index of transitive dlg
    deriving (Show)

instance RocksBatchOp DelegationOp where
    toBatchOp (PskFromEdgeAction (DlgEdgeAdd psk))
        | isRevokePsk psk =
          error $ "RocksBatchOp DelegationOp: malformed " <>
                  "revoke psk in DlgEdgeAdd: " <> pretty psk
        | not (verifyPsk psk) =
          error $ "Tried to insert invalid psk: " <> pretty psk
        | otherwise =
          [Rocks.Put (pskKey $ addressHash $ pskIssuerPk psk) (encode psk)]
    toBatchOp (PskFromEdgeAction (DlgEdgeDel issuerPk)) =
        [Rocks.Del $ pskKey issuerPk]
    toBatchOp (AddTransitiveDlg iSId dSId) =
        [Rocks.Put (transDlgKey iSId) (encode dSId)]
    toBatchOp (DelTransitiveDlg sId) =
        [Rocks.Del $ transDlgKey sId]
    toBatchOp (SetTransitiveDlgRev dSId iSIds)
        | HS.null iSIds = [Rocks.Del $ transRevDlgKey dSId]
        | otherwise     = [Rocks.Put (transRevDlgKey dSId) (encode iSIds)]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- Transitive relation iteration
data DlgTransRevIter

instance DBIteratorClass DlgTransRevIter where
    type IterKey DlgTransRevIter = StakeholderId
    type IterValue DlgTransRevIter = HashSet StakeholderId
    iterKeyPrefix = iterTransRevPrefix

-- | For each stakeholder, say who has delegated to that stakeholder
-- (basically iterate over transitive reverse relation).
--
-- NB. It's not called @getIssuers@ because we already have issuers (i.e.
-- block issuers)
getDelegators :: MonadDBRead m => Source (ResourceT m) (StakeholderId, HashSet StakeholderId)
getDelegators = dbIterSource GStateDB (Proxy @DlgTransRevIter)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

-- Storing Hash IssuerPk -> ProxySKHeavy
pskKey :: StakeholderId -> ByteString
pskKey s = "d/p/" <> encode s

transDlgKey :: StakeholderId -> ByteString
transDlgKey s = "d/t/" <> encode s

iterTransRevPrefix :: ByteString
iterTransRevPrefix = "d/tr/"

transRevDlgKey :: StakeholderId -> ByteString
transRevDlgKey = encodeWithKeyPrefix @DlgTransRevIter
