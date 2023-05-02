module MultiSet 
            (MultiSet , emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
      where 

data MultiSet a = M (Map a Int)

import MapV1 

-- Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a
emptyMS = M (emptyM)

-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (M map) = assocM x ((cantidadQueAparece x map)+1) map

cantidadQueAparece :: Ord a => a -> Map a Int -> Int
cantidadQueAparece x map = 
      case lookupM x map of 
            Nothing -> 0
            Just n  -> n 


-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (M map) = cantidadQueAparece x map

-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
unionMS (M map) (M map1) = M (unir (keys map) map map1)

unir :: Ord a => [k] -> Map a Int -> Map a Int -> Map a Int
unir [] _ map2        = map2
unir (k:ks) map1 map2 = assocM k (fromJust (lookupM k map1))  (unir ks map1 map2)   



-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
intersectionMS (M map1) (M map2) = M (interseccion (keys map1) map2) 

interseccion :: [k] -> Map a Int -> Map a Int
interseccion [] map     = emptyM 
interseccion (k:ks) map = 
      case lookupM k map of
            Nothing -> interseccion ks map 
            Just v  -> assocM k v (interseccion ks map )

-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (M map) = combinar (keys map) map 

combinar :: Ord a => [a] -> Map a Int -> [(a, Int)]
combinar [] map     = []
combinar (x:xs) map = (x, (cantidadQueAparece x map)) : combinar xs map 

