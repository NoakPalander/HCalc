module Parser where

import Token
import Tokenizer
import Stack
import Error
import Converter (toRpn)
import Data.Either (fromRight)


-- Applies an operator on two operands, or returns a parse error
binaryOp :: Token -> Double -> Double -> Either ParseError Double
binaryOp Div 0 _ = Left $ PError "Cannot divide by zero"
binaryOp t x y = case t of
  Add -> Right $ y + x
  Sub -> Right $ y - x
  Mul -> Right $ y * x
  Div -> Right $ y / x
  _ -> Left $ PError ("Invalid binary operator " ++ show t)

unaryOp :: Token -> Double -> Either ParseError Double
unaryOp (Unary Sub) x = Right (-x)
unaryOp (Unary Add) x = Right x
unaryOp t _   = Left $ PError ("Invalid unary operator: " ++ show t)

type DStack = Stack Double

evalUnaryOp :: DStack -> Token -> Either ParseError DStack
evalUnaryOp (Stack (x:y:ys)) t = sPush (Stack (y:ys)) <$> unaryOp t x
evalUnaryOp (Stack (x:xs)) t = sPush (Stack xs) <$> unaryOp t x
evalUnaryOp (Stack _) t = Left $ PError "Cannot evaluate unary operator with arity 0"

-- Evaluates an operator given two operands
evalBinaryOp :: DStack -> Token -> Either ParseError DStack
evalBinaryOp (Stack (x:y:ys)) t = sPush (Stack ys) <$> binaryOp t x y
evalBinaryOp (Stack _) t = Left $ PError "Cannot evaluate binary operator with arity 0"

-- Evaluates a single number
evalNum :: DStack -> Token -> Either ParseError DStack
evalNum s (Literal n) = Right $ sPush s (fromIntegral n)
evalNum _ _ = Left $ PError "Cannot evaluate a non-number as a number"

-- Evaluates an RPN expression
evalRpn :: DStack -> Tokens -> Either ParseError Double
evalRpn s [] = maybe (Left $ PError "Empty stack error") Right $ sPeek s
evalRpn s (t:ts)
  | isUnary t    = either Left (`evalRpn` ts) $ evalUnaryOp s t
  | isOperator t = either Left (`evalRpn` ts) $ evalBinaryOp s t
  | isNumber t   = either Left (`evalRpn` ts) $ evalNum s t
  | otherwise    = case t of
      OpenParens -> Left $ PError "("
      CloseParens -> Left $ PError ")"
      _ -> Left $ PError "_"

-- Parses an RPN expression
parse :: Tokens -> Either ParseError Double
parse = evalRpn emptyS

-- Attempts to evaluate an expression
eval :: String -> Either ParseError Double
eval expr = do
  tokens <- mapE PError $ tokenize expr
  rpn <- mapE PError $ toRpn tokens
  parse rpn
