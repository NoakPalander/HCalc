module Converter (toRpn) where

import Token
import Tokenizer
import Queue
import Stack
import Error

type Cache = (Queue Token, Stack Token)


-- Reorders the expression into reverse notation
reorder :: Cache -> Tokens -> Either LexError Cache
reorder cache [] = Right cache
reorder cache@(queue, stack) (n:ns)
  | n == OpenParens  = reorder (queue, sPush stack OpenParens) ns
  | n == CloseParens = either Left (`reorder` ns) $ popParens cache n
  | isNumber n       = reorder (qPush queue n, stack) ns
  | isOperator n     = reorder (popOps cache n) ns
  | otherwise        = Right cache

-- Pops operators
popOps :: Cache -> Token -> Cache
popOps (q, s) o1
  | sLength s <= 0 = stop
  | o2 == OpenParens = stop
  | isOperator o2 && o2 > o1 || (samePrec && lAssoc) = popOps (qPush q o2, sPop s) o1
  | otherwise = stop
  where
    o2 = sTop s
    lAssoc = associativity o1 == ALeft
    samePrec = precedence o2 == precedence o1
    stop = (q, sPush s o1)

-- Pops parentheses
popParens :: Cache -> Token -> Either LexError Cache
popParens (q, s) t
  | sLength s <= 0      = Left $ LError "Mismatched parens"
  | o /= OpenParens     = popParens (qPush q o, sPop s) t
  | otherwise           = Right (q, sPop s)
  where
   o = sTop s

-- Runs the shunting yard algorithm
shunting :: Cache -> Either LexError Tokens
shunting (queue, Stack []) = Right $ qToList queue
shunting (queue, Stack (o:os))
  | o /= OpenParens     = shunting (qPush queue o, Stack os)
  | otherwise           = Left $ LError "Mismatched parens"

-- Converts tokens to RPN
toRpn :: Tokens -> Either LexError Tokens
toRpn ts = either Left shunting $ reorder (Queue [], Stack []) ts
