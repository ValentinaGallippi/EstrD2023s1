module PriorityQueueV1
        (PriorityQueue, emptyPQ, isEmptyPQ , insertPQ, findMinPQ, deleteMinPQ) 
     where

data PriorityQueue a = PQ [a]
{- INV. REP : en PQ xs 
             * los elementos de xs estan ordenados de menor a mayor. -}
emptyPQ :: PriorityQueue a -- O(1)
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool -- O(1)
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(n)
-- xs esta ordenada de mayor a menor
insertPQ x (PQ xs) = PQ (agregarEnOrden x xs)
 
findMinPQ :: Ord a => PriorityQueue a -> a --O(n)
findMinPQ (PQ xs) = head xs 

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a -- O(n)
deleteMinPQ (PQ xs) = PQ (tail xs)
 

agregarEnOrden :: Ord a => a -> [a] -> [a] -- O(n)
agregarEnOrden x []= [x]
agregarEnOrden x (y:ys) = meterOrdenado y (agregarEnOrden x ys)

meterOrdenado :: Ord a => a -> [a] -> [a] --O(1)
--la lista dada tiene elemento 
meterOrdenado y (x:xs) = if y > x  then  x : (y : xs)
                                    else y : (x : xs)