module Main where

import Tokenizer
import Converter
import System.IO

-- TODO: Write unit tests for toRpn
main :: IO ()
main = do
  putStr "Enter an expression > "
  hFlush stdout

  input <- getLine
  let tokens = tokenize input
  putStrLn $ "Tokens: " ++ show tokens
  putStrLn $ "RPN: " ++ show (toRpn tokens)
