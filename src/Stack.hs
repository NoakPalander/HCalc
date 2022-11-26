module Stack where

newtype Stack a = Stack [a]
  deriving (Ord, Eq, Show)


emptyS :: Stack a
emptyS = Stack []

isEmptyStack :: Stack a -> Bool
isEmptyStack (Stack []) = True
isEmptyStack (Stack _) = False

-- Pushes a value ontop of the stakc
sPush :: Stack a -> a -> Stack a
sPush (Stack xs) x = Stack $ x:xs

-- Pushes a list on top of the stack
sExtend :: Stack a -> [a] -> Stack a
sExtend (Stack xs) ys = Stack $ ys ++ xs

-- Pops the top element of the stack
sPop :: Stack a -> Stack a
sPop (Stack []) = Stack []
sPop (Stack (x:xs)) = Stack xs

-- Returns the top element of the stack
sTop :: Stack a -> a
sTop (Stack []) = error "Cannot access the top on an empty stack"
sTop (Stack (x:_)) = x

-- Returns the top element of the stack if it has one
sPeek :: Stack a -> Maybe a
sPeek (Stack []) = Nothing
sPeek (Stack (x:_)) = Just x

-- Returns the length of the stack
sLength :: Stack a -> Int
sLength (Stack xs) = length xs

-- Converts the stack into its underlying list
sToList :: Stack a -> [a]
sToList (Stack xs) = xs
