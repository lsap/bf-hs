{-# LANGUAGE RankNTypes #-}

module Tape where

import Control.Monad.Reader
import Control.Monad.ST
import Data.Array.ST (MArray (getBounds), STArray, modifyArray, newArray, readArray, writeArray)
import Data.STRef
import DiffList

type Runtime s = (STRef s [Int], STRef s (DiffList Int), TapeArray s, STRef s Int)

type TapeArray s = STArray s Int Int

type TapeAction a s = Runtime s -> ST s a

type TapeM s a = ReaderT (Runtime s) (ST s) a

isTrue :: TapeM s Bool
isTrue = do
  (_, _, arr, idx) <- ask
  lift $ do
    idx' <- readSTRef idx
    (/= 0) <$> readArray arr idx'

incr :: TapeM s ()
incr = do
  (_, _, arr, idx) <- ask
  lift $ do
    idx' <- readSTRef idx
    modifyArray arr idx' (+ 1)

decr :: TapeM s ()
decr = do
  (_, _, arr, idx) <- ask
  lift $ do
    idx' <- readSTRef idx
    modifyArray arr idx' (subtract 1)

goRight :: TapeM s ()
goRight = do
  (_, _, arr, idxRef) <- ask
  lift $ do
    (min', max') <- getBounds arr
    idx <- readSTRef idxRef
    if idx < max' then modifySTRef idxRef (+ 1) else writeSTRef idxRef min'

goLeft :: TapeM s ()
goLeft = do
  (_, _, arr, idxRef) <- ask
  lift $ do
    (min', max') <- getBounds arr
    idx <- readSTRef idxRef
    if idx > min' then modifySTRef idxRef (subtract 1) else writeSTRef idxRef max'

stdin :: TapeM s ()
stdin = do
  (inp, _, arr, idxRef) <- ask
  lift $ do
    inp' <- readSTRef inp
    case inp' of
      [] -> error "No input"
      x : xs -> do
        writeSTRef inp xs
        idx <- readSTRef idxRef
        writeArray arr idx x

stdout :: TapeM s ()
stdout = do
  (_, out, arr, idxRef) <- ask
  lift $ do
    idx <- readSTRef idxRef
    val <- readArray arr idx
    modifySTRef out (appendItem val)

getRuntime :: [Int] -> ST s (Runtime s)
getRuntime input = do
  idx <- newSTRef 0
  input' <- newSTRef input
  arr <- newArray (0, 10) 0 :: ST s (TapeArray s)
  output <- newSTRef mempty :: ST s (STRef s (DiffList Int))
  pure (input', output, arr, idx)