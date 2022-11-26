module Main where

import Tokenizer
import Converter
import System.IO
import Control.Monad
import Data.Either (either)


printTokens :: String -> IO ()
printTokens expr = either print puts $ tokenize expr
  where puts t = putStrLn (expr ++ ": " ++ show t)

main :: IO ()
main = printTokens "15 + 1 * 3"
