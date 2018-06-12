module Lib
    ( transfer
    , debit
    , credit
    ) where

import Control.Concurrent.STM
import Data.Decimal

type Account = TVar Decimal

debit :: Account -> Decimal -> STM ()
debit acc v = do
  s <- readTVar acc
  writeTVar acc (s - v)
  pure ()

credit :: Account -> Decimal -> STM ()
credit acc v = do
  s <- readTVar acc
  writeTVar acc (s + v)
  pure ()

transfer :: Account -> Account -> Decimal -> STM ()
transfer from to amount = do
  debit  from amount
  credit to   amount

transfer2 :: Account -> Account -> Decimal -> STM ()
transfer2 from to amount = orElse actualTransfer (pure ())
  where
    actualTransfer = do
      debit  from amount
      credit to   amount
