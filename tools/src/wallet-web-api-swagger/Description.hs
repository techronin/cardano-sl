{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Descriptions for each endpoint, for Swagger-documentation.

module Description () where

import           Universum

import           Control.Lens                       ((?~))
import           Data.Swagger                       (Operation, Swagger, description)
import           Servant                            ((:>))
import           Servant.Swagger                    (HasSwagger, subOperations)
import           Servant.Swagger.Internal.TypeLevel (IsSubAPI)

import           CustomSwagger                      (HasCustomSwagger (..))
import           Instances                          ()
import qualified Pos.Wallet.Web                     as W

-- | Wallet API operations, i.e. modifier of part of api related to
-- single endpoint.
wop
    :: forall sub.
       ( IsSubAPI (W.ApiPrefix :> sub) W.WalletApi
       , HasSwagger (W.ApiPrefix :> sub)
       )
    => Proxy sub -> Traversal' Swagger Operation
wop _ = subOperations (Proxy @(W.ApiPrefix :> sub)) W.walletApi

modifyDescription
    :: (IsSubAPI (W.ApiPrefix :> api) W.WalletApi
       , HasSwagger api
       )
    => Text -> Proxy api -> Swagger -> Swagger
modifyDescription desc api = wop api . description ?~ desc


instance HasCustomSwagger api => HasCustomSwagger (W.ApiPrefix :> api) where
    docModifier _ = docModifier (Proxy @api)

instance HasCustomSwagger W.TestReset where
    docModifier = modifyDescription
        "Delete all secret keys. It works in development mode only, \
        \returns HTTP 403 otherwise."

instance HasCustomSwagger W.GetWallet where
    docModifier = modifyDescription
        "Get information about a wallet by its ID (address)."

instance HasCustomSwagger W.GetWallets where
    docModifier = modifyDescription
        "Get information about all available wallets."

instance HasCustomSwagger W.NewWallet where
    docModifier = modifyDescription
        "Create a new wallet."

instance HasCustomSwagger W.UpdateWallet where
    docModifier = modifyDescription
        "Update wallet's meta information."

instance HasCustomSwagger W.RestoreWallet where
    docModifier = modifyDescription
        "Create a new wallet."

instance HasCustomSwagger W.RenameWallet where
    docModifier = modifyDescription
        "Change name of given wallet."

instance HasCustomSwagger W.DeleteWallet where
    docModifier = modifyDescription
        "Delete given wallet with all contained accounts."

instance HasCustomSwagger W.ImportWallet where
    docModifier = modifyDescription
        "Import user's secret key from the path to generate wallet."

instance HasCustomSwagger W.ChangeWalletPassphrase where
    docModifier = modifyDescription
        "Change passphrase of given wallet."


instance HasCustomSwagger W.GetAccount where
    docModifier = modifyDescription
        "Get information about a account by account's ID \
        \(address + index of account in wallet)."

instance HasCustomSwagger W.GetAccounts where
    docModifier = modifyDescription
        "Get information about all available accounts."

instance HasCustomSwagger W.UpdateAccount where
    docModifier = modifyDescription
        "Update account's meta information."

instance HasCustomSwagger W.NewAccount where
    docModifier = modifyDescription
        "Create a new account in given wallet."

instance HasCustomSwagger W.DeleteAccount where
    docModifier = modifyDescription
        "Delete a account by account's ID (address + index of \
        \account in wallet)."


instance HasCustomSwagger W.NewAddress where
    docModifier = modifyDescription
        "Create a new address in given account."


instance HasCustomSwagger W.IsValidAddress where
    docModifier = modifyDescription
        "Returns True if given address is valid, False otherwise."


instance HasCustomSwagger W.GetProfile where
    docModifier = modifyDescription
        "Get user profile's meta data."

instance HasCustomSwagger W.UpdateProfile where
    docModifier = modifyDescription
        "Update user profile."


instance HasCustomSwagger W.NewPayment where
    docModifier = modifyDescription
        "Create a new payment transaction."

instance HasCustomSwagger W.TxFee where
    docModifier = modifyDescription
        "Estimate fees for performing given transaction. \
        \Transaction will not be created."

instance HasCustomSwagger W.UpdateTx where
    docModifier = modifyDescription
        "Update payment transaction."

instance HasCustomSwagger W.GetHistory where
    docModifier = modifyDescription
        "Get the history of transactions."


instance HasCustomSwagger W.NextUpdate where
    docModifier = modifyDescription
        "Get information about the next update."

instance HasCustomSwagger W.ApplyUpdate where
    docModifier = modifyDescription
        "Apply last update."


instance HasCustomSwagger W.RedeemADA where
    docModifier = modifyDescription
        "Redeem ADA."

instance HasCustomSwagger W.RedeemADAPaperVend where
    docModifier = modifyDescription
        "Redeem ADA, paper vending."


instance HasCustomSwagger W.ReportingInitialized where
    docModifier = modifyDescription
        "Send node's report on initialization time."

instance HasCustomSwagger W.ReportingElectroncrash where
    docModifier = modifyDescription
        "Send node's report on electron crash info."


instance HasCustomSwagger W.GetSlotsDuration where
    docModifier = modifyDescription
        "Get blockchain slot duration in milliseconds."

instance HasCustomSwagger W.GetVersion where
    docModifier = modifyDescription
        "Get current version of the node."

instance HasCustomSwagger W.GetSyncProgress where
    docModifier = modifyDescription
        "Sync progress, with info about local chain difficulty,\
        \network chain difficulty and connected peers."


instance HasCustomSwagger W.ImportBackupJSON where
    docModifier = modifyDescription
        "Import full information about wallets from a given file."

instance HasCustomSwagger W.ExportBackupJSON where
    docModifier = modifyDescription
        "Export full information about wallets in JSON format into a file under \
        \given path. Wallets may be later restored with 'ImportBackupJSON' \
        \endpoint."
