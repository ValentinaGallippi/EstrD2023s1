module RAL 
    (RAList , emptyRAL, isEmptyRAL, lengthRAL, get, minRAL, add, elems, remove, set, addAt)
where 

import Heap
import MapV1


data RAList a = MkR Int (Map Int a) (Heap a)
    deriving Show 

{-
INV.REP: 
    Sea (MkR n mi h)
    * si n = 0 no hay ningun elemento en mi ni en h (dudoso, es lo mismo q el de abajo)
    * En mi y h tiene que haber n cantidad de elementos 
    * todos los valores del map estan asociados al n dado al momento de asociarse. 

-}

-- Propósito: devuelve una lista vacía.
-- Eficiencia: O(1).
emptyRAL :: RAList a
emptyRAL = MkR 0 emptyM emptyH

-- Propósito: indica si la lista está vacía.
-- Eficiencia: O(1).
isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MkR i mi h) = i == 0 

lengthRAL :: RAList a -> Int
-- Propósito: devuelve la cantidad de elementos.
-- Eficiencia: O(1).
lengthRAL (MkR i mi h) = (i-1)

get :: Int -> RAList a -> a
-- Propósito: devuelve el elemento en el índice dado.
-- Precondición: el índice debe existir.
-- Eficiencia: O(log N).
get i (MkR i' mi h) = 
    case lookupM i mi of 
        Nothing -> error "el indice no existe"
        Just v  -> v 

-- Propósito: devuelve el mínimo elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(1).
minRAL :: Ord a => RAList a -> a
minRAL (MkR i mi h) = findMin h 

-- Propósito: agrega un elemento al final de la lista.
-- Eficiencia: O(log N).
add :: Ord a => a -> RAList a -> RAList a
add x (MkR i mi h) = MkR (i+1) (assocM i x mi) (insertH x h)

-- Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
-- Eficiencia: O(N log N).
elems :: Ord a => RAList a -> [a]
elems (MkR i mi h) = reversa (elementosDe i mi)

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa  xs) x 

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     x = [x]
agregarAlFinal (x:xs) y = x : (agregarAlFinal xs y)

-- preguntar: 
--   0 no tendria un elemento? porque cuando ingreso un elemento a un RAL vacio se asocia 0 con el elemento, entonces como hago??
elementosDe :: Int -> Map Int a -> [a] 
elementosDe 0 mi = []
elementosDe i mi = fromJust (lookupM i mi) : elementosDe (i-1) mi 
   
fromJust :: Maybe v -> v 
fromJust (Just v) = v 


-- Propósito: elimina el último elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(N log N).
remove :: Ord a => RAList a -> RAList a
remove (MkR i mi h) = 
    let elemABorrar = fromJust (lookupM (i-1) mi) 
        newH        = borrar elemABorrar h 
        newM        = deleteM (i-1) mi 
    in (MkR (i-1) newM newH)

borrar :: Ord a => a -> Heap a -> Heap a 
borrar x h = 
    if findMin h == x then deleteMin h
                      else insertH (findMin h) (borrar x (deleteMin h))  


-- Propósito: reemplaza el elemento en la posición dada.
-- Precondición: el índice debe existir.
-- Eficiencia: O(N log N).
set :: Ord a => Int -> a -> RAList a -> RAList a
set i x (MkR i' mi h) = let newM = assocM i x mi 
                            newH = reemplazarPorXEn x (elemento i mi) h
                        in (MkR i' newM newH)

reemplazarPorXEn :: Ord a => a -> a -> Heap a -> Heap a 
reemplazarPorXEn x y h = insertH x (borrar x h)

elemento:: Int -> Map Int a ->  a 
-- Precondición: el índice debe existir.
elemento i mi = 
    case lookupM i mi of 
        Nothing -> error " no existe dicho indice"
        Just v  -> v 


-- Propósito: agrega un elemento en la posición dada.
-- Precondición: el índice debe estar entre 0 y la longitud de la lista.
-- Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
-- Eficiencia: O(N log N).
-- Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
-- también como argumento la máxima posición posible.
addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt i x (MkR i' mi h) = 
        let elementoAReemplazar = fromJust (lookupM i mi )
            newH = insertH x (borrar elementoAReemplazar h)
            newM = correrDesdeElementos i i' mi
        in MkR (i'+1) (assocM i x newM)  newH

correrDesdeElementos :: Int -> Int -> Map Int a -> Map Int a
correrDesdeElementos i i' mi = 
    if i == i' then mi
               else assocM (i+1) (fromJust (lookupM i mi) ) (deleteM i (correrDesdeElementos (i+1) i' mi))

    