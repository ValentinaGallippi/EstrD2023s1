module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
where


data Map k v = M [(k,v)]
     -- clave, valor 
        deriving Show 


emptyM :: Map k v -- O(1)
emptyM = M []

assocM :: Ord k => k -> v -> Map k v -> Map k v -- O(n)
assocM k v (M kvs) = M ((k,v) : kvs)


-- O(n) 
lookupM :: Ord k => k -> Map k v -> Maybe v
lookupM k (M kvs) = buscarEn k kvs


--O(n)
buscarEn :: Ord k => k -> [(k,v)] -> Maybe v 
buscarEn k [] = Nothing 
buscarEn k ((k',v) : kvs) = if k == k' then Just v 
                                       else buscarEn k kvs 

-- O(n)
deleteM :: Ord k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (sinLaClave k kvs)

--O(n)
sinLaClave :: Ord k => k -> [(k,v)] -> [(k,v)]
sinLaClave k [] = []
sinLaClave k ((k',v) : kvs) = if k == k' then sinLaClave k kvs
                                        else (k',v) : (sinLaClave k kvs)

--O(n)
keys :: Ord k => Map k v -> [k]
keys (M kvs) = clavesSinRepetirDe kvs

--O(n)
clavesSinRepetirDe :: Ord k => [(k,v)] -> [k]
clavesSinRepetirDe [] = []
clavesSinRepetirDe ((k,v):kvs) = if not (estaEn k kvs) then  k : clavesSinRepetirDe kvs
                                           else clavesSinRepetirDe kvs 


estaEn :: Ord k=> k -> [(k,v)] -> Bool
estaEn  k' []     = False
estaEn  k' ((k,v):kvs) = k == k' || estaEn k' kvs 