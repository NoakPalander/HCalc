module Error where


newtype LexError = LError String
  deriving Eq

instance Show LexError where
  show (LError e) = e


newtype ParseError = PError String
  deriving Eq

instance Show ParseError where
  show (PError e) = e
