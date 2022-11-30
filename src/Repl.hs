module Repl (repl) where

import Data.Either (either, isLeft, fromRight, fromLeft)
import System.IO
import Error
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
  if line == "exit" then do
    return ()
    else do
      either logError logResult $ eval line
      repl
