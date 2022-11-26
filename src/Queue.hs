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
qFront :: Queue a -> Maybe a
qFront (Queue []) = Nothing
qFront (Queue (x:xs)) = Just x

-- Returns the last item of the queue if it has one
qBack :: Queue a -> Maybe a
qBack (Queue []) = Nothing
qBack (Queue xs) = Just $ last xs

-- Returns the length of the queue
qLength :: Queue a -> Int
qLength (Queue xs) = length xs

-- Converts the queue into its undelying list
qToList :: Queue a -> [a]
qToList (Queue xs) = xs
