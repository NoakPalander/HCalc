module Tokenizer
  ( Token(..)
  , Tokens
  , Assoc(..)
  , isToken
  , isOperator
  , isNumber
  , toString
  , toToken
  , assoc
  , precedence
  , tokenize
  ) where

import Prelude hiding (traverse)
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Data.List (isInfixOf)

-- Token types
data Token
  = OpenParens
  | CloseParens
  | Add
  | Sub
  | Mul
  | Div
  | Literal Int
  deriving (Show, Eq)

instance Ord Token where
  t1 > t2   = precedence t1 > precedence t2
  t1 >= t2  = precedence t1 >= precedence t2
  t1 < t2   = precedence t1 < precedence t2
  t1 <= t2  = precedence t1 <= precedence t2

precedence :: Token -> Int
precedence t = case t of
  Add -> 2
  Sub -> 2
  Mul -> 3
  Div -> 3
  _ -> error $ "Only operators have a precedence, received an " ++ show t

data Assoc = ALeft | ARight deriving Eq

assoc :: Token -> Assoc
assoc t
  | isOperator t  = ALeft
  | otherwise     = ARight

-- Returns a true if the character is ignorable
ignorable :: Char -> Bool
ignorable = isSpace

-- Returns true if the string is a valid token
isToken :: String -> Bool
isToken s = s `isInfixOf` "()+-*/" || isJust (readMaybe s :: Maybe Int)

isOperator :: Token -> Bool
isOperator = flip isInfixOf "+-*/" . toString

isNumber :: Token -> Bool
isNumber (Literal _)  = True
isNumber _            = False

-- Stringify's a token
toString :: Token -> String
toString t = case t of
    OpenParens -> "("
    CloseParens -> ")"
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Literal n -> show n

-- Converts a string to a token if it's valid
toToken :: String -> Maybe Token
toToken s = case s of
  "(" -> Just OpenParens
  ")" -> Just CloseParens
  "+" -> Just Add
  "-" -> Just Sub
  "*" -> Just Mul
  "/" -> Just Div
  _ ->  fmap Literal (readMaybe s)

type Tokens = [Token]
type Cache = ([Token], [Char])

-- Returns an empty cache
empty :: Cache
empty = ([], [])

-- Evaluates a token and possibly its next token, and keeps track of a cache
eval :: Cache -> Char -> Maybe Char -> Cache
eval cache@(ts, keys) c cn
  | isDigit c = num cache c cn
  | otherwise = case toToken [c] of
      Just t -> (ts ++ [t], [])
      Nothing -> if ignorable c then cache else error $ "Invalid token: " ++ [c]

  where
    addCurrent :: Cache
    addCurrent = (ts ++ [fromJust (toToken (keys ++ [c]))], keys)

    num :: Cache -> Char -> Maybe Char -> Cache
    num (tokens, keys) c next = case next of
      Nothing -> addCurrent
      Just n -> if not (isDigit n) then addCurrent else (tokens, keys ++ [c])

-- Iterates over the expression and returns the caches with tokens in reversed order
traverse :: String -> Cache -> Cache
traverse [] cache = cache
traverse (x:y:ys) cache = traverse (y:ys) (eval cache x $ Just y)
traverse (x:xs) cache = traverse xs (eval cache x Nothing)

-- Traverses the expression and returns a list of tokens
tokenize :: String -> Tokens
tokenize s = fst $ traverse s empty
