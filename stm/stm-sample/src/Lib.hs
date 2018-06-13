module Lib
    ( transfer
    , transfer2
    , debit
    , credit
    ) where

import Control.Concurrent.STM
import Data.Decimal
import Control.Concurrent.Async

type Account = TVar Integer

debit :: Account -> Integer -> STM ()
debit acc v = do
  s <- readTVar acc
  if s >= v
    then writeTVar acc (s - v)
    else retry

credit :: Account -> Integer -> STM ()
credit acc v = do
  s <- readTVar acc
  writeTVar acc (s + v)

transfer :: Account -> Account -> Integer -> STM ()
transfer from to amount = do
  debit  from amount
  credit to   amount

transfer2 :: Account -> Account -> Integer -> STM ()
transfer2 from to amount = actualTransfer `orElse` pure ()
  where
    actualTransfer = do
      debit  from amount
      credit to   amount

{- 
3. Are our invariants really invariant?
Create a third bank account, to serve as our "bank". Give it an insane amount of money.

Add a dependency on the async and random packages. Import Control.Concurrent.Async and try to find whatever you need from the random package.

Create a 5 threads using race_ (excersise in itself). Spawn the following function:
-}

thread :: Int -> [Account] -> IO ()
thread numTransactions accounts = undefined
