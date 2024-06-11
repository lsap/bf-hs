{-# LANGUAGE LambdaCase #-}

module BF (runBF) where

import Control.Monad (forM_, when)
import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef
import DiffList
import Parser
import Tape

runBF :: String -> [Int] -> Maybe [Int]
runBF source input = do
  bf <- parseBF' source
  pure $ runBF' bf input

runBF' :: [BF] -> [Int] -> [Int]
runBF' bf input = runST $ do
  runtime@(_, output, _, _) <- getRuntime input
  runReaderT (act bf) runtime
  toList <$> readSTRef output

whileM :: (Monad m) => m Bool -> m a -> m ()
whileM p f = do
  isit <- p
  when isit $ f *> whileM p f

act :: [BF] -> ReaderT (Runtime s) (ST s) ()
act chunk = forM_ chunk mapBFToAction

mapBFToAction :: BF -> ReaderT (Runtime s) (ST s) ()
mapBFToAction = \case
  Inc -> incr
  Dec -> decr
  GoLeft -> goLeft
  GoRight -> goRight
  Write -> stdin
  Read -> stdout
  Loop chunk' -> whileM isTrue (act chunk')
