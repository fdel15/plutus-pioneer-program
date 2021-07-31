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

module Week05.NFT where

import           Data.Aeson            (FromJSON, ToJSON)
import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics          (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

data NFTParams = NFTParams
    { frankToken :: TokenName
    , numberOfTokens  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
-- function take a token name argument (i changed to NFTParams)
mint np = do
    let tn = frankToken np
        num = numberOfTokens np
    -- bind the contracts public key to a variable
    pk    <- Contract.ownPubKey
    -- bind all UTXOs associated with the public key address to a variable
    utxos <- utxoAt (pubKeyAddress pk)
    -- map over all unspent transactions
    case Map.keys utxos of
        -- if unspent transactions is an empty list return an error
        []       -> Contract.logError @String "no utxo found"
        -- if unspent transactions exist, grab the first one (oref)
        oref : _ -> do
            -- Make a 'Value' containing only the given quantity of the given currency and bind it to val 
            let val     = Value.singleton (curSymbol oref tn) tn num
            --  lookups value with minting policy script <> scripts lookup that uses unspent outputs to resolve input constraints
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                -- constraint that we must mint 1 token <> monoid txconstraint mustSpendPubKeyOutput
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            -- submit transaction
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            -- wait for response from submission
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    callEndpoint @"mint" h1 $ NFTParams
        { frankToken = "Frank"
        , numberOfTokens = 1  
        }
    callEndpoint @"mint" h2 $ NFTParams
        { frankToken = "Bob"
        , numberOfTokens = 1  
        }
    void $ Emulator.waitNSlots 1
