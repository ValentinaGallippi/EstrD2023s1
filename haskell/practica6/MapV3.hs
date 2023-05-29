module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
where


data Map k v = M [k] [v]
     -- clave, valor 
{-Inv Rep: las listas siempre tienen la misma cantidad de elementos y la clave ubicada en la posición i está
asociada al valor en la misma posición, pero de la otra lista. -}
            deriving Show 


emptyM :: Map k v -- O(1)
emptyM = M [] []

assocM :: Ord k => k -> v -> Map k v -> Map k v -- O(n)
assocM k v (M ks vs) = M (k:ks) (v:vs)


-- O(n) 
lookupM :: Ord k => k -> Map k v -> Maybe v
lookupM k (M ks vs) = buscarEn k ks vs

buscarEn :: Ord k => k -> [k] -> [v] -> Maybe v
buscarEn k [] [] = Nothing
buscarEn k (k':ks) (v:vs) = if k == k' then Maybe v 
                                       else buscarEn k ks vs


-- O(n)
deleteM :: Ord k => k -> Map k v -> Map k v
deleteM k (M ks vs) = M (sinLaClave k ks) (sinElValorDe k ks vs)

sinLaClave :: Ord k => k -> [k] ->  [k]
sinLaClave k []     = []
sinLaClave k (k':ks) = if  k == k' then ks 
                                   else k' : (sinLaClave k ks)

sinElValorDe :: Ord k => k-> [k] -> [v] -> [v]
sinElValorDe k [] [] = []
sinElValorDe k (k':ks) (v:vs) = if k == k' then vs
                                           else v : (sinElValorDe k ks vs) 

--O(n)
keys :: Ord k => Map k v -> [k]
keys (M ks vs) = clavesSinRepetirDe ks

--O(n)
clavesSinRepetirDe :: Ord k => [k] -> [k]
clavesSinRepetirDe [] = []
clavesSinRepetirDe (k:ks) = if not (estaEn k ks) then  k : clavesSinRepetirDe ks
                                           else clavesSinRepetirDe ks 


estaEn :: Ord k=> k -> [k] -> Bool
estaEn  k' []     = False
estaEn  k' (k:ks) = k == k' || estaEn k' ks 


