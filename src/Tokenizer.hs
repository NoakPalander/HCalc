module Tokenizer (Tokens, tokenize) where

import Prelude hiding (traverse)
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Data.List (isInfixOf)
import Data.Either
import Queue
import Stack
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
compose :: Cache -> Char -> Token
compose (tokens, keys) c = fromJust $ toToken (qToList (qPush keys c))

-- Composes a complete number given it's stored cache ,current and next token
composeNum :: Cache -> Char -> Maybe Char -> Cache
composeNum cache@(tokens, keys) key Nothing = pushFirst (`compose` key) cache
composeNum cache@(tokens, keys) key (Just next)
  | isDigit next  = pushSecond (const key) cache
  | otherwise     =  pushFirst (`compose` key) cache

-- Evaluates a token and possibly its next token, and keeps track of a cache
eval :: Cache -> Char -> Maybe Char -> Either LexError Cache
eval cache@(tokens, keys) key next
  | isDigit key = Right $ composeNum cache key next
  | otherwise = maybe invalid (Right . add) $ toToken [key]
  where
    invalid = Left . LError $ "Invalid token: " ++ [key]
    add t = pushFirst (const t) (tokens, emptyQ)

-- Iterates over the expression and returns the caches with tokens in reversed order
traverse :: String -> Cache -> Either LexError Cache
traverse [] c = Right c
traverse (x:y:ys) c = either Left (traverse (y:ys)) $ eval c x (Just y)
traverse (x:xs) c = either Left (traverse xs) $ eval c x Nothing

-- Traverses the expression and returns a list of tokens
tokenize :: String -> Either LexError Tokens
tokenize [] = Right []
tokenize s  = qToList . fst <$> traverse stripped empty
  where stripped = filter (not . ignorable) s
