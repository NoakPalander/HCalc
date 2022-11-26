module Main where

import Tokenizer
import Parser
import Converter

import Data.Either (either, fromRight)
import Stack


main :: IO ()
main = do
  let tokens = either (error "Tokenization failure") id $ tokenize "1 / 0"
  let rpn = toRpn tokens
  print $ parse rpn
