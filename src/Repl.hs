module Repl (repl) where

import Data.Char (toLower)
import Data.Either (either)
import System.IO
import Parser


logError :: Show a => a -> IO ()
logError e = hPutStrLn stderr $ "Error: " ++ show e

logResult :: Double -> IO ()
logResult r = putStrLn $ "> " ++ show r

repl :: IO ()
repl = do
  putStr "λ "
  hFlush stdout
  line <- getLine
  if map toLower line == "exit" then do
    return ()
    else do
      either logError logResult $ eval line
      repl
