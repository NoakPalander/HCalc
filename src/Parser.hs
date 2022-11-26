module Parser where

import Token
import Tokenizer
import Stack
import Error
import Converter (toRpn)
import Data.Either (fromRight)

type DStack = Stack Double

-- Applies an operator on two operands, or returns a parse error
applyOp :: Token -> Double -> Double -> Either ParseError Double
applyOp Div 0 _ = Left $ PError "Cannot divide by zero"
applyOp t x y = case t of
  Add -> Right $ y + x
  Sub -> Right $ y - x
  Mul -> Right $ y * x
  Div -> Right $ y / x
  _ -> Left $ PError "Invalid operator to apply"

-- Evaluates an operator given two operands
evalOp :: DStack -> Token -> Either ParseError DStack
evalOp (Stack (x:y:ys)) t = sPush (Stack ys) <$> applyOp t x y
evalOp _ _ = Left $ PError "Cannot evaluate a binary expression with less than 2 operands"

-- Evaluates a single number
evalNum :: DStack -> Token -> Either ParseError DStack
evalNum s (Literal n) = Right $ sPush s (fromIntegral n)
evalNum _ _ = Left $ PError "Cannot evaluate a non-number as a number"

-- Evaluates an RPN expression
evalRpn :: DStack -> Tokens -> Either ParseError Double
evalRpn s [] = maybe (Left $ PError "Empty stack error") Right $ sFront s
evalRpn s (t:ts)
  | isOperator t = either Left (`evalRpn` ts) $ evalOp s t
  | isNumber t   = either Left (`evalRpn` ts) $ evalNum s t
  | otherwise    = case t of
      OpenParens -> Left $ PError "("
      CloseParens -> Left $ PError ")"
      _ -> Left $ PError "_"

-- Parses an RPN expression
parse :: Tokens -> Either ParseError Double
parse = evalRpn emptyS
