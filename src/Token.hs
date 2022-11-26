module Token where

import Text.Read (readMaybe)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Char (isSpace)

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

-- Returns a true if the character is ignorable
ignorable :: Char -> Bool
ignorable = isSpace

-- TODO: Return some enum ordering thingy?
-- Returns the precedence of a token if it has one
precedence :: Token -> Maybe Int
precedence t = case t of
  Add -> Just 2
  Sub -> Just 2
  Mul -> Just 3
  Div -> Just 3
  _ -> Nothing

-- TODO: This is pretty ugly
data Associativity = ALeft | ARight
  deriving Eq

-- Returns true if the
associativity :: Token -> Associativity
associativity t
  | isOperator t  = ALeft
  | otherwise     = ARight

-- Returns true if the string is a valid token
isToken :: String -> Bool
isToken s = s `isInfixOf` "()+-*/" || isJust (readMaybe s :: Maybe Int)

-- Returns true if the token is an operator
isOperator :: Token -> Bool
isOperator = flip isInfixOf "+-*/" . toString

-- Returns true if the token is a number
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