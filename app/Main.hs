module Main where

import Tokenizer
import Parser
import Converter

import Data.Either (either, isLeft, fromRight, fromLeft)
import Stack
import System.IO
import Error (ParseError(PError))

logError :: Show a => a -> IO ()
logError e = hPutStrLn stderr $ "Error: " ++ show e

logResult :: Double -> IO ()
logResult r = putStrLn $ "> " ++ show r

main :: IO ()
main = do
  putStr "λ "
  hFlush stdout
  line <- getLine
  if line == "exit" then do
    return ()
    else do
      either logError logResult $ eval line
      main
