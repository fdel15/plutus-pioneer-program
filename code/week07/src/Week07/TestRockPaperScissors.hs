{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.TestRockPaperScissors where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Ledger
import           Ledger.TimeSlot
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet

import           Week07.RockPaperScissors

test :: IO ()
test = do
  test' Rock Paper
  test' Rock Scissors
  test' Rock Rock
  test' Paper Rock
  test' Paper Scissors
  test' Paper Paper
  test' Scissors Rock
  test' Scissors Paper
  test' Scissors Scissors

test' :: GameChoice -> GameChoice -> IO ()
test' choice1 choice2 = runEmulatorTraceIO $ myTrace choice1 choice2

myTrace :: GameChoice -> GameChoice -> EmulatorTrace ()
myTrace choice1 choice2 = do
  Extras.logInfo $ "first move: " ++ show choice1 ++ ", second move: " ++ show choice2

  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints

  let pkh1 = pubKeyHash $ walletPubKey $ Wallet 1
      pkh2 = pubKeyHash $ walletPubKey $ Wallet 2
      stake = 5_000_000
      deadline1 = slotToEndPOSIXTime def 5
      deadline2 = slotToEndPOSIXTime def 10

      fp = FirstParams
        { fpSecond          = pkh2
        , fpStake           = stake
        , fpPlayDeadline    = deadline1
        , fpRevealDeadline  = deadline2
        , fpNonce           = "SECRETNONCE"
        , fpChoice          = choice1
        }

  callEndpoint @"first" h1 fp

  threadToken <- getThreadToken h1

  let sp = SecondParams
              { spFirst          = pkh1
              , spStake          = stake
              , spPlayDeadline   = deadline1
              , spRevealDeadline = deadline2
              , spChoice         = choice2
              , spToken          = threadToken
              }

  void $ Emulator.waitNSlots 3

  callEndpoint @"second" h2 sp

  void $ Emulator.waitNSlots 10
 where
  getThreadToken :: ContractHandle (Last ThreadToken) GameSchema Text -> EmulatorTrace ThreadToken
  getThreadToken handle = do
    void $ Emulator.waitNSlots 1
    Last m <- observableState handle
    case m of
      Nothing -> getThreadToken handle
      Just threadToken -> Extras.logInfo ("read thread token " ++ show threadToken) >> return threadToken
