module Main where

import Tokenizer
import Token
import Parser
import Converter
import Data.Either (either, isLeft, fromRight, fromLeft)
import Stack
import System.IO
import Error (ParseError(PError))

main :: IO ()
main = either print print $ tokenize "5 - (-4)"
