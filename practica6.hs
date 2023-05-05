import PriorityQueueV1
--(PriorityQueue, emptyPQ, isEmptyPQ , insertPQ, findMinPQ, deleteMinPQ)

import MapV1
--(Map, emptyM, assocM, lookupM, deleteM, keys)

import MultiSet
-- (MultiSet , emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)

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

----------------------------------------------------------------------------------------------------------------------------------------
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Ord k => Map k v -> [Maybe v]
valuesM map = valores (keys map) map

valores :: Ord k => [k] -> Map k v -> [Maybe v]
valores [] map      = []
valores (k:ks) map  = lookupM k map : valores ks  map

-- Propósito: indica si en el map se encuentran todas las claves dadas
todasAsociadas :: Ord k => [k] -> Map k v -> Bool
todasAsociadas claves map = estanTodasLasClaves claves (keys map) 

estanTodasLasClaves :: Ord k => [k] -> [k] -> Bool
estanTodasLasClaves xs []     = False
estanTodasLasClaves [] ys     = False
estanTodasLasClaves (x:xs) ys = estaEn x ys && estanTodasLasClaves xs ys  


estaEn :: Ord k  => k -> [k] -> Bool
estaEn k (y:ys) = k==y || estaEn k ys 


-- Propósito: convierte una lista de pares clave valor en un map.
listToMap ::Ord k  => [(k, v)] -> Map k v
listToMap []             = emptyM
listToMap ((k,v) : kvs ) = assocM k v (listToMap kvs)

-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Ord k => Map k v -> [(k, v)]
mapToList map = asociarClavesCon (keys map) map 

asociarClavesCon :: Ord k => [k] -> Map k v -> [(k,v)]  
asociarClavesCon []     map  = []
asociarClavesCon (k:ks) map  = (k,fromJust (lookupM k map)) : asociarClavesCon ks map



fromJust :: Maybe v -> v
--PRECONDICION: NO PUEDE SER NOTHING. 
fromJust (Just v) = v


-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
-- la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v] -- se usa case Cuando el resultado de una subtarea es un tipo algebraico, y no amerita otra subtarea 
agruparEq []            = emptyM
agruparEq ((k,v) : kvs) =  
        case lookupM k (agruparEq kvs) of 
          Nothing -> assocM k (v:[]) (agruparEq kvs) 
          Just vs  -> assocM k (v:vs) (agruparEq kvs ) 



-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
incrementar :: Ord k => [k] -> Map k Int -> Map k Int
incrementar []     map = map
incrementar (k:ks) map = 
    case lookupM k map of 
        Nothing -> incrementar ks map
        Just v  -> assocM k (v+1) (incrementar ks map) 
   

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps:: Ord k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = agregarClavesDeA (keys map1) map1 map2 

agregarClavesDeA :: Ord k =>[k] -> Map k v -> Map k v -> Map k v
agregarClavesDeA []     _  m2 = m2
agregarClavesDeA (k:ks) m1 m2 = assocM k (fromJust(lookupM k m1)) (agregarClavesDeA ks m1 m2)

-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
indexar :: [a] -> Map Int a
indexar xs = indexarDesde 0 xs

indexarDesde :: Int -> [a] -> Map Int a
indexarDesde n [] = emptyM
indexarDesde n (x:xs) = assocM n x (indexarDesde (n+1) xs)

map :: Map Char Int
map = 
    assocM 'r' 1
    (assocM 'a' 1
    (assocM 'm' 1  
    (emptyM))) 

map1 :: Map Char Int
map1 = assocM 'v' 7 
    (assocM 'r' 24
    (assocM 'a' 4
    (assocM 'v' 13  
    (emptyM))) )

-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias []     = emptyM
ocurrencias (c:cs) = assocM c (vecesQueApareceEn c cs) (ocurrencias cs)

vecesQueApareceEn ::Eq c => c -> [c] -> Int
vecesQueApareceEn c [] = 1
vecesQueApareceEn c (c':cs) = if c == c' then (vecesQueApareceEn c cs) + 1 
                                         else vecesQueApareceEn c 
                                         

ocurrencias' :: String -> MultiSet Char
ocurrencias' []     = emptyMS
ocurrencias' (c:cs) = addMS c (vecesQueApareceEn c cs) (ocurrencias' cs)

