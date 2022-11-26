module Converter (toRpn) where

import Token
import Tokenizer

type Queue = [Token]
type Stack = [Token]
type Cache = (Queue, Stack)

split :: Cache -> Tokens -> Cache
split cache [] = cache
split cache@(queue, stack) (n:ns)
  | n == OpenParens     = split (queue, OpenParens:stack) ns
  | n == CloseParens    = split (popParens cache n) ns
  | isNumber n          = split (queue ++ [n], stack) ns
  | isOperator n        = split (popOps cache n) ns
  | otherwise           = cache

-- TODO: fix this
popOps :: Cache -> Token -> Cache
popOps (q, o2:os) o1
  | o2 == OpenParens = stop
  | isOperator o2 && o2 /= OpenParens && o2 > o1 || ((precedence o2 == precedence o1) && associativity o1 == ALeft) = popOps (q ++ [o2], os) o1
  | otherwise = stop
  where
    stop = (q, o1:o2:os)

popOps (queue, stack) op = (queue, op:stack)

popParens :: Cache -> Token -> Cache
popParens (q, o:os) t
  | o /= OpenParens     = popParens (q ++ [o], os) t
  | o == OpenParens     = (q, os)
  | otherwise           = error "Mismatched parens"

popParens (q, []) _ = error "Mismatched parens"

shunting :: Cache -> Tokens
shunting (queue, []) = queue
shunting (queue, o:os)
  | o /= OpenParens     = shunting (queue ++ [o], os)
  | otherwise           = error "Mismatched parens"

toRpn :: Tokens -> Tokens
toRpn tokens = shunting $ split ([], []) tokens
