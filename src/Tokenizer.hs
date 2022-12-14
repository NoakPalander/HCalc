module Tokenizer (Tokens, tokenize) where

import Prelude hiding (traverse)
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Data.List (isInfixOf)
import Data.Either (either)
import Queue
import Token
import Error

type Cache = (Queue Token, Queue Char)

-- Pushes to the first queue in the cache given a function
pushFirst :: (Cache -> Token) -> Cache -> Cache
pushFirst func c@(f, s) = (qPush f (func c), s)

-- Pushes to the second queue in the cache given a function
pushSecond :: (Cache -> Char) -> Cache -> Cache
pushSecond func c@(f, s) = (f, qPush s (func c))

-- Returns an empty cache
empty :: Cache
empty = (emptyQ, emptyQ)

-- Composes a number given its current token and stored cache
compose :: Cache -> Char -> Either LexError Token
compose (ts, ks) c = maybe (Left $ LError "Invalid token sequence") Right comp
  where
    comp = toToken $ qToList (qPush ks c)

-- Composes a complete number given it's stored cache ,current and next token
composeNum :: Cache -> Char -> Maybe Char -> Either LexError Cache
composeNum cache@(ts, _) k Nothing = (\c -> pushFirst (const c) cache) <$> compose cache k
composeNum cache@(ts, _) k (Just n)
  | isDigit n = Right $ pushSecond (const k) cache
  | otherwise =  (\c -> pushFirst (const c) cache) <$> compose cache k

-- Composes a unary operator
composeUnary :: Cache -> Char -> Either LexError Cache
composeUnary q k = either Left (\t -> Right $ pushFirst (const t) q) token
  where
    token = maybe (Left . LError $ "Invalid token " ++ [k]) (Right . Unary) $ toToken [k]

-- Evaluates a token and possibly its next token, and keeps track of the cache
eval :: Cache -> Char -> Maybe Char -> Either LexError Cache
eval cache@(ts, ks) k n
  | isDigit k = composeNum cache k n
  | isParens k = maybe invalid (Right . add) $ toToken [k]
  | otherwise = do
      -- Required conditions for a token to be matched as a unary operator
      if qLength ts == 0 || (isOperator prev || prev == OpenParens) && isStringOp [k] then
        composeUnary cache k
      else maybe invalid (Right . add) $ toToken [k]
  where
    invalid = Left . LError $ "Invalid token: " ++ [k]
    prev = qBack ts
    add t = pushFirst (const t) (ts, emptyQ)

-- Iterates over the expression and returns the caches with tokens
traverse :: String -> Cache -> Either LexError Cache
traverse [] c = Right c
traverse (x:y:ys) c = either Left (traverse (y:ys)) $ eval c x (Just y)
traverse (x:xs) c = either Left (traverse xs) $ eval c x Nothing

-- Traverses the expression and returns a list of infix tokens
tokenize :: String -> Either LexError Tokens
tokenize [] = Right []
tokenize s  = qToList . fst <$> traverse stripped empty
  where stripped = filter (not . ignorable) s
