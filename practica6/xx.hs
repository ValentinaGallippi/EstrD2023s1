import MapV2



indexar :: [a] -> Map Int a
indexar xs = indexarDesde 0 xs

indexarDesde :: Int -> [a] -> Map Int a
indexarDesde n [] = emptyM
indexarDesde n (x:xs) = assocM n x (indexarDesde (n+1) xs)