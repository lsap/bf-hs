module Main where

import BF

main :: IO ()
main = do
  let program = ",>,[-<+>]<."
  case runBF program [2, 4] of
    Just output -> print output
    Nothing -> putStrLn "Error"
