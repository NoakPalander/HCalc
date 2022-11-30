module Operators where

import Error
import Token

-- Applies an operator on two operands, or returns a parse error
binaryOp :: Token -> Double -> Double -> Either ParseError Double
binaryOp Div 0 _ = Left $ PError "Cannot divide by zero"
binaryOp t x y = case t of
  Add -> Right $ y + x
  Sub -> Right $ y - x
  Mul -> Right $ y * x
  Div -> Right $ y / x
  Exp -> Right $ y ** x
  _ -> Left $ PError ("Invalid binary operator " ++ show t)

unaryOp :: Token -> Double -> Either ParseError Double
unaryOp (Unary Sub) x = Right (-x)
unaryOp (Unary Add) x = Right x
unaryOp t _   = Left $ PError ("Invalid unary operator: " ++ show t)
