{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.Reader (ask, lift, runReaderT, ReaderT)
import Control.Monad.ST (runST, ST)
import Data.STRef (modifySTRef, newSTRef, readSTRef, writeSTRef, STRef)
import Data.Array.ST (MArray (getBounds), STArray, modifyArray, newArray, readArray, writeArray)
newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

instance Semigroup (DiffList a) where
  (<>) (DiffList l1) (DiffList l2) = DiffList (l1 . l2)

instance Monoid (DiffList a) where
  mempty = DiffList id
type Runtime s = (STRef s [Int], STRef s (DiffList Int), TapeArray s, STRef s Int)

type TapeArray s = STArray s Int Int

type TapeAction a s = Runtime s -> ST s a

type TapeM s a = ReaderT (Runtime s) (ST s) a
data BF
  = GoLeft
  | GoRight
  | Inc
  | Dec
  | Write
  | Read
  | Loop [BF]
  deriving (Show, Eq)
runBF :: String -> [Int] -> Maybe [Int]
runBF source input = do
  bf <- parseBF' source
  pure $ runBF' bf input

runBF' :: [BF] -> [Int] -> [Int]
runBF' bf input = runST $ do 
  runtime@(_, output, _, _) <- getRuntime input
  runReaderT (act bf) runtime
  toList <$> readSTRef output

parseBF' :: String -> Maybe [BF]
parseBF' s = fst <$> parseBF s 0
prepend :: a -> ([a], b) -> ([a], b)
prepend x (c, xs) = (x : c, xs)

parseBF :: String -> Int -> Maybe ([BF], String)
parseBF "" 0 = Just ([], "")
parseBF "" _ = Nothing
parseBF (x : xs) d = case x of
  '>' -> prepend GoRight <$> parseBF xs d
  '<' -> prepend GoLeft <$> parseBF xs d
  '+' -> prepend Inc <$> parseBF xs d
  '-' -> prepend Dec <$> parseBF xs d
  ',' -> prepend Write <$> parseBF xs d
  '.' -> prepend Read <$> parseBF xs d
  '[' -> do
    (contents, xs') <- parseBF xs (d + 1)
    prepend (Loop contents) <$> parseBF xs' d
  ']' -> if d == 0 then Nothing else Just ([], xs)
  _ -> parseBF xs d
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
appendItem :: a -> DiffList a -> DiffList a
appendItem x (DiffList f) = DiffList (f . (x :))

toList :: DiffList a -> [a]
toList (DiffList f) = f []

fromList :: [a] -> DiffList a
fromList xs = DiffList (xs ++)
main :: IO ()
main = do
  let program = ",>,[-<+>]<."
  case runBF program [2, 4] of
    Just output -> print output
    Nothing -> putStrLn "Error"

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

