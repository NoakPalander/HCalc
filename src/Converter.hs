module Converter (toRpn) where

import Token
import Tokenizer
import Queue
import Stack

type Cache = (Queue Token, Stack Token)

-- Reorders the expression into reverse notation
reorder :: Cache -> Tokens -> Cache
reorder cache [] = cache
reorder cache@(queue, stack) (n:ns)
  | n == OpenParens     = reorder (queue, sPush stack OpenParens) ns
  | n == CloseParens    = reorder (popParens cache n) ns
  | isNumber n          = reorder (qPush queue n, stack) ns
  | isOperator n        = reorder (popOps cache n) ns
  | otherwise           = cache

-- TODO: fix this
popOps :: Cache -> Token -> Cache
popOps (q, s) o1
  | sLength s <= 0 = stop
  | o2 == OpenParens = stop
  | isOperator o2 && o2 /= OpenParens && o2 > o1 || ((precedence o2 == precedence o1) && associativity o1 == ALeft) = popOps (qPush q o2, sPop s) o1
  | otherwise = stop
  where
    o2 = sTop s
    stop = (q, sPush s o1)

-- Pops parentheses
popParens :: Cache -> Token -> Cache
popParens (q, s) t
  | sLength s <= 0      = error "Mismatched parens"
  | o /= OpenParens     = popParens (qPush q o, sPop s) t
  | otherwise           = (q, sPop s)
  where
   o = sTop s

shunting :: Cache -> Tokens
shunting (queue, Stack []) = qToList queue
shunting (queue, Stack (o:os))
  | o /= OpenParens     = shunting (qPush queue o, Stack os)
  | otherwise           = error "Mismatched parens"

toRpn :: Tokens -> Tokens
toRpn tokens = shunting $ reorder (Queue [], Stack []) tokens
