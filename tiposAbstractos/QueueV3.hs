module QueueV3
        (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where 

--               Front Stock
data Queue a = Q [a] [a]
--                   Back stock
-- Inv .Rep.  : Si fs se encuentra vacía, entonces la cola se encuentra vacía
emptyQ :: Queue a
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs ys) = null xs 

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs ys) = if null xs then Q (x:xs) [] 
                                 else Q xs (agregarAlFinal ys x)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     x = [x]
agregarAlFinal (x:xs) y = x : (agregarAlFinal xs y)

firstQ :: Queue a -> a
firstQ (Q xs ys) = head xs

dequeue :: Queue a -> Queue a
dequeue (Q xs ys) = if null ys then emptyQ
                               else Q [head ys] (tail ys)

