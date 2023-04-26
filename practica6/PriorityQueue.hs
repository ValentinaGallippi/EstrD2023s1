module PriorityQueue
        (PriorityQueue, emptyPQ, isEmptyPQ , insertPQ, findMinPQ, deleteMinPQ) 
     where

data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a -- O(1)
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool -- O(1)
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(1)
insertPQ x (PQ xs) = PQ (x:xs) 
 
findMinPQ :: Ord a => PriorityQueue a -> a --O(n)
findMinPQ (PQ xs) = minimum xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a -- O(n)
deleteMinPQ (PQ xs) = PQ (sacar (minimun xs) xs) 

sinElMinimo :: Ord a  => [a] -> [a]  -- O(n)
sinElMinimo []     = []
sinElMinimo (x:xs) = if minimum xs == x then xs
                                        else x : sinElMinimo xs
