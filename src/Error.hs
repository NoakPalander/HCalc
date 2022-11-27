module Error where
import Control.Arrow (ArrowChoice(left))


newtype LexError = LError String
  deriving Eq

instance Show LexError where
  show (LError e) = e

newtype ParseError = PError String
  deriving Eq

instance Show ParseError where
  show (PError e) = e

newtype EvalError = EError String
  deriving Eq

instance  Show EvalError where
  show (EError e) = e

-- Converts an error type through string, to another error type
mapE :: Show a => (String -> b) -> Either a c -> Either b c
mapE f = left (f . show)
