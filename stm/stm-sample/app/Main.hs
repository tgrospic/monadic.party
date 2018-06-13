module Main where

import Lib
import Control.Concurrent.STM
import Data.Decimal
import Data.Maybe
import Control.Arrow
import Control.Monad
import Data.Bifunctor
import System.Random

main :: IO ()
main = sample1 *> sample2 *> randomDecimalPrint

account1, account2, bank :: STM (TVar Integer)
account1 = newTVar 50
account2 = newTVar 100
bank     = newTVar 100000

sample1 = do
  a1 <- atomically account1
  a2 <- atomically account2
  val <- (randomIO :: IO Integer)
  print ("Random", val)
  -- Do transfer
  atomically $ transfer a1 a2 val
  -- Get values
  a1' <- readTVarIO a1
  a2' <- readTVarIO a2
  -- Print
  print (a1', a2')

sample2 = atomically tr >>= print
  where
    tr = join $ transferWithResult 49 <$> account1 <*> account2
    -- Transfer
    transferWithResult v a1 a2 = do
      transfer2 a1 a2 v
      x1 <- readTVar a1
      x2 <- readTVar a2
      pure (x1, x2)

-- randomInt max = do
--   g <- getStdRandom (randomR (1,6))

randomDecimalPrint = do
  g <- getStdGen
  print $ take 2 (randoms g :: [Double])
