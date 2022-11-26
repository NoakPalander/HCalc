module Queue where


newtype Queue a = Queue [a]
  deriving (Ord, Eq, Show)


emptyQ :: Queue a
emptyQ = Queue []

isEmptyStack :: Queue a -> Bool
isEmptyStack (Queue []) = True
isEmptyStack (Queue _) = False

-- Pushes an item to the end of the queue
qPush :: Queue a -> a -> Queue a
qPush (Queue xs) x = Queue $ xs ++ [x]

-- Pushes multiple items into the queue
qExtend :: Queue a -> [a] -> Queue a
qExtend (Queue xs) ys = Queue $ xs ++ ys

-- Pops the first element of the queue
qPop :: Queue a -> Queue a
qPop (Queue []) = Queue []
qPop (Queue (_:xs)) = Queue xs

-- Returns the first element of the queue if it has one
qPeek :: Queue a -> Maybe a
qPeek (Queue []) = Nothing
qPeek (Queue (x:_)) = Just x

-- Returns the first element of the queue
qFront :: Queue a -> a
qFront (Queue []) = error "Cannot access the front element on an empty queue"
qFront (Queue (x:_)) = x

-- Returns the last element of the queue if it has one
qLast :: Queue a -> a
qLast (Queue []) = error "Cannot access the back element on an empty queue"
qLast (Queue xs) = last xs

-- Returns the last item of the queue if it has one
qBack :: Queue a -> a
qBack (Queue []) = error "Cannot access the back element on an empty queue"
qBack (Queue xs) = last xs

-- Returns the length of the queue
qLength :: Queue a -> Int
qLength (Queue xs) = length xs

-- Converts the queue into its undelying list
qToList :: Queue a -> [a]
qToList (Queue xs) = xs
