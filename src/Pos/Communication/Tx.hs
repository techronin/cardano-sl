-- | Functions for operating with transactions

{-# LANGUAGE RankNTypes #-}

module Pos.Communication.Tx
       ( TxMode
       , submitTx
       , submitMTx
       , submitRedemptionTx
       , submitTxRaw
       , sendTxOuts
       ) where

import           Formatting                 (build, sformat, (%))
import           Mockable                   (MonadMockable)
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary                 ()
import           Pos.Client.Txp.Balances    (MonadBalances (..), getOwnUtxo)
import           Pos.Client.Txp.History     (MonadTxHistory (..))
import           Pos.Client.Txp.Util        (TxError (..), createMTx, createRedemptionTx,
                                             createTx)
import           Pos.Communication.Methods  (sendTx)
import           Pos.Communication.Protocol (NodeId, OutSpecs, EnqueueMsg)
import           Pos.Communication.Specs    (createOutSpecs)
import           Pos.Communication.Types    (InvOrDataTK)
import           Pos.Crypto                 (RedeemSecretKey, SafeSigner, hash,
                                             redeemToPublic, safeToPublic)
import           Pos.DB.Class               (MonadGState)
import           Pos.Txp.Core               (TxAux (..), TxId, TxOut (..), TxOutAux (..),
                                             txaF)
import           Pos.Txp.Network.Types      (TxMsgContents (..))
import           Pos.Types                  (Address, Coin, makePubKeyAddress,
                                             makeRedeemAddress, mkCoin, unsafeAddCoin)
import           Pos.Util.Util              (eitherToThrow)
import           Pos.WorkMode.Class         (MinWorkMode)

type TxMode ssc m
    = ( MinWorkMode m
      , MonadBalances m
      , MonadTxHistory ssc m
      , MonadMockable m
      , MonadMask m
      , MonadGState m
      , MonadThrow m
      )

submitAndSave
    :: TxMode ssc m
    => EnqueueMsg m -> TxAux -> m TxAux
submitAndSave enqueue txAux@TxAux {..} = do
    let txId = hash taTx
    submitTxRaw enqueue txAux
    saveTx (txId, txAux)
    return txAux

-- | Construct Tx using multiple secret keys and given list of desired outputs.
submitMTx
    :: TxMode ssc m
    => EnqueueMsg m
    -> NonEmpty (SafeSigner, Address)
    -> NonEmpty TxOutAux
    -> m TxAux
submitMTx enqueue hdwSigner outputs = do
    let addrs = map snd $ toList hdwSigner
    utxo <- getOwnUtxos addrs
    txw <- eitherToThrow TxError $
           createMTx utxo hdwSigner outputs
    submitAndSave enqueue txw

-- | Construct Tx using secret key and given list of desired outputs
submitTx
    :: TxMode ssc m
    => EnqueueMsg m
    -> SafeSigner
    -> NonEmpty TxOutAux
    -> m TxAux
submitTx enqueue ss outputs = do
    utxo <- getOwnUtxos . one $ makePubKeyAddress (safeToPublic ss)
    txw <- eitherToThrow TxError $
           createTx utxo ss outputs
    submitAndSave enqueue txw

-- | Construct redemption Tx using redemption secret key and a output address
submitRedemptionTx
    :: TxMode ssc m
    => EnqueueMsg m
    -> RedeemSecretKey
    -> Address
    -> m (TxAux, Address, Coin)
submitRedemptionTx enqueue rsk output = do
    let redeemAddress = makeRedeemAddress $ redeemToPublic rsk
    utxo <- getOwnUtxo redeemAddress
    let addCoin c = unsafeAddCoin c . txOutValue . toaOut
        redeemBalance = foldl' addCoin (mkCoin 0) utxo
        txouts = one $
            TxOutAux {toaOut = TxOut output redeemBalance, toaDistr = []}
    when (redeemBalance == mkCoin 0) $
        throwM . TxError $ "Redeem balance is 0"
    txw <- eitherToThrow TxError $
           createRedemptionTx utxo rsk txouts
    txAux <- submitAndSave enqueue txw
    pure (txAux, redeemAddress, redeemBalance)

-- | Send the ready-to-use transaction
submitTxRaw
    :: (MinWorkMode m, MonadGState m, MonadThrow m)
    => EnqueueMsg m -> TxAux -> m ()
submitTxRaw enqueue txAux@TxAux {..} = do
    let txId = hash taTx
    logInfo $ sformat ("Submitting transaction: "%txaF) txAux
    logInfo $ sformat ("Transaction id: "%build) txId
    sendTx enqueue txAux

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK TxId TxMsgContents))
