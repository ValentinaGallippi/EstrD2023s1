module QueueV2
        (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where 
data Queue a = Q [a]

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs 

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (x:xs)

firstQ :: Queue a -> a
firstQ (Q xs) = last xs

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (sinElUltimo xs)

sinElUltimo :: [a] -> [a]
sinElUltimo []     = []
sinElUltimo (x:xs) = if null xs then sinElUltimo xs 
                                else x : (sinElUltimo xs)
