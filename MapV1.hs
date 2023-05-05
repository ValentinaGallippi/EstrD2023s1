module MapV1
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
where


data Map k v = M [(k,v)]
     -- clave, valor 
{- Inv. Rep : en M [(k,v)] , k no esta repetida. -}
      deriving Show 

      
emptyM :: Map k v -- O(1)
emptyM = M []

assocM :: Ord k => k -> v -> Map k v -> Map k v -- O(n)
assocM k v (M kvs) = M (agregarSinRepetir k v kvs)

-- O(n)
agregarSinRepetir :: Ord k =>  k -> v -> [(k,v)] -> [(k,v)] -- la lista de pares no tiene ninguna clave repetida. 
agregarSinRepetir k v [] = (k,v) : []
agregarSinRepetir k v ((k2,v2) : kvs ) =  if k == k2 then (k,v)   : kvs
                                                     else (k2,v2) : (agregarSinRepetir k v kvs)


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
sinLaClave k ((k',v) : kvs) = if k == k' then kvs
                                        else (k',v) : (sinLaClave k kvs)

--O(n)
keys :: Ord k => Map k v -> [k]
keys (M kvs) = clavesDe kvs

--O(n)
clavesDe :: Ord k => [(k,v)] -> [k]
clavesDe [] = []
clavesDe ((k,v):ks) = k : clavesDe ks
