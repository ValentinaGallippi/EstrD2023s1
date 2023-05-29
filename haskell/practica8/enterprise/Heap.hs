module Heap(Heap,emptyH,isEmptyH,insertH,findMin,deleteMin)where

data Heap a = H [a] (Maybe a)  deriving Show
-- INV.REP:
-- Sea (H xs m) una Heap:
-- * m es el elemento mas prioritario

-- Devuelve una Heap vacía.
emptyH :: Heap a -- O(1)
emptyH = H [] Nothing

-- Indica si la Heap está vacía.
isEmptyH :: Heap a -> Bool -- O(1)
isEmptyH (H xs _) = null xs
-- isEmptyH (H _ m) = m == Nothing

-- Inserta un elemento en la Heap.
insertH :: Ord a => a -> Heap a -> Heap a -- O(1)
insertH x (H xs m) = H (x:xs) (masPrioritario x m)

-- Devuelve el elemento más prioriotario (el mínimo) de la Heap.
-- PREC: parcial en caso de Heap vacía.
findMin :: Ord a => Heap a -> a -- O(1)
findMin (H _ m) = fromJust m

-- Devuelve una Heap sin el elemento más prioritario (el mínimo).
-- PREC: parcial en caso de Heap vacía.
-- O(n) donde n es la longitud de la Heap
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (H xs _) = let xs' = removeOne (findMinL xs) xs
                         in H xs' (Just (findMinL xs'))


-- ------------------ | SUBTAREAS | ------------------

-- O(1)
masPrioritario :: Ord a => a -> Maybe a -> Maybe a
masPrioritario x Nothing   = Just x
masPrioritario x (Just x') = Just (min x x')

-- O(1)
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "Nothing"

-- O(n) donde n es la longitud de la lista
findMinL :: Ord a => [a] -> a
findMinL []     = error "No hay elementos"
findMinL [x]    = x
findMinL (x:xs) = min x (findMinL xs)

-- O(n) donde n es la longitud de la lista
removeOne :: Eq a => a -> [a] -> [a]
removeOne _ []     = []
removeOne e (x:xs) = if e==x then removeOne e xs else x : removeOne e xs

