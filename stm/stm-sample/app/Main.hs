module Main where

import Lib
import Control.Concurrent.STM
import Data.Decimal
import Control.Arrow
import Control.Monad
import Data.Bifunctor

main :: IO ()
main = sample1 *> sample2

account1 = newTVar 50.0
account2 = newTVar 100.0

sample1 = do
  a1 <- atomically account1
  a2 <- atomically account2
  -- Do transfer
  atomically $ transfer a1 a2 9
  -- Get values
  a1' <- readTVarIO a1
  a2' <- readTVarIO a2
  -- Print
  print (a1', a2')

sample2 = atomically tr >>= print
  where
    tr = join $ transferWithResult 9 <$> account1 <*> account2
    -- Transfer
    transferWithResult v a1 a2 = do
      transfer a1 a2 v
      x1 <- readTVar a1
      x2 <- readTVar a2
      pure (x1, x2)

