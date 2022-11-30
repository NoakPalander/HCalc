module Parser where

import Token
import Tokenizer
import Stack
import Error
import Converter (toRpn)
import Data.Either (fromRight)
import Operators


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
evalNum _ t = Left . PError $ "Cannot evaluate " ++ show t ++ " as a number"

-- Evaluates an RPN expression
evalRpn :: DStack -> Tokens -> Either ParseError Double
evalRpn s [] = maybe (Left $ PError "Empty stack error") Right $ sPeek s
evalRpn s (t:ts)
  | isUnary t    = either Left (`evalRpn` ts) $ evalUnaryOp s t
  | isOperator t = either Left (`evalRpn` ts) $ evalBinaryOp s t
  | isNumber t   = either Left (`evalRpn` ts) $ evalNum s t
  | otherwise    = case t of
      OpenParens -> Left $ PError "Reverse polish notation cannot have opening parens"
      CloseParens -> Left $ PError "Reverse polish notation cannot have closing parens"
      _ -> Left $ PError "Unknown parse error"

-- Parses an RPN expression
parse :: Tokens -> Either ParseError Double
parse = evalRpn emptyS

-- Attempts to evaluate an expression
eval :: String -> Either ParseError Double
eval expr = do
  tokens <- mapE PError $ tokenize expr
  rpn <- mapE PError $ toRpn tokens
  parse rpn
