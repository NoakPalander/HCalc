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

type DStack = Stack Double

-- Evaluates an operator given two operands
evalOp :: DStack -> Token -> Either ParseError DStack
evalOp (Stack (x:y:ys)) t = sPush (Stack ys) <$> binaryOp t x y
evalOp (Stack _) t = Left $ PError "Cannot evaluate an operator with less than 2 args"

-- Evaluates a single number
evalNum :: DStack -> Token -> Either ParseError DStack
evalNum s (Literal n) = Right $ sPush s (fromIntegral n)
evalNum _ _ = Left $ PError "Cannot evaluate a non-number as a number"

-- Evaluates an RPN expression
evalRpn :: DStack -> Tokens -> Either ParseError DStack
evalRpn s [] = Right s -- maybe (Left $ PError "Empty stack error") Right $ sPeek s
evalRpn s (t:ts)
  | isOperator t = evalOp s t -- either Left (`evalRpn` ts) $ evalOp s t
  | isNumber t   = evalNum s t -- either Left (`evalRpn` ts) $ evalNum s t
  | otherwise    = case t of
      OpenParens -> Left $ PError "("
      CloseParens -> Left $ PError ")"
      _ -> Left $ PError "_"

-- Parses an RPN expression
{-
parse :: Tokens -> Either ParseError Double
parse = evalRpn emptyS

-- Attempts to evaluate an expression
eval :: String -> Either ParseError Double
eval expr = do
  tokens <- mapE PError $ tokenize expr
  rpn <- mapE PError $ toRpn tokens
  parse rpn
-}

testEval :: IO ()
testEval = do
  print $ evalRpn (Stack [4.0, 5.0]) [Sub, Sub]
