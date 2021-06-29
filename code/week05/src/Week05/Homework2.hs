{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
tn :: TokenName
tn = TokenName emptyByteString

-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> ScriptContext -> Bool
mkPolicy oref ctx = 
  traceIfFalse "utxo not consumed" hasUTXo           &&
  traceIfFalse "wrong amount"      checkMintedAmount
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   hasUTXo :: Bool
   hasUTXo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

   checkMintedAmount :: Bool
   checkMintedAmount = case flattenValue (txInfoForge info) of
     [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
     _                -> False

policy :: TxOutRef -> Scripts.MonetaryPolicy
policy oref = mkMonetaryPolicyScript $
 $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
 `PlutusTx.applyCode`
 PlutusTx.liftCode oref

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type NFTSchema =
    BlockchainActions
        .\/ Endpoint "mint" ()

mint :: Contract w NFTSchema Text ()
mint = do
  -- look up pk
  pk     <- Contract.ownPubKey
  -- find utxos from pubKeyAddress
  utxos  <- utxoAt (pubKeyAddress pk)
  -- Map over keys to find oref
  case Map.keys utxos of
    []  -> Contract.logError @String "no utxo found"
    oref: _ -> do
    --   -- get value, lookups, and tx
    let value = Value.singleton (curSymbol oref) tn 1
        lookups = Constraints.monetaryPolicy (policy oref) <> Constraints.unspentOutputs utxos
        tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
    -- submit transaction
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    -- wait for confirmation
    void $ awaitTxConfirmed $ txId ledgerTx
    -- do some logging
    Contract.logInfo @String $printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >> mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 ()
    callEndpoint @"mint" h2 ()
    void $ Emulator.waitNSlots 1
