import PriorityQueueV1

--(PriorityQueue, emptyPQ, isEmptyPQ , insertPQ, findMinPQ, deleteMinPQ)

-- dada una lista la ordena de
-- menor a mayor utilizando una Priority Queue como estructura auxiliar.
heapSort :: Ord a => [a] -> [a]
heapSort xs = ordenado (listaAPQ xs )

ordenado ::  Ord a => PriorityQueue a -> [a]
ordenado pq = if isEmptyPQ pq then []
                              else findMinPQ pq : ordenado (deleteMinPQ pq)

listaAPQ ::  Ord a =>  [a] -> PriorityQueue a
listaAPQ []     =  emptyPQ 
listaAPQ (x:xs) =  insertPQ x  (listaAPQ xs)
