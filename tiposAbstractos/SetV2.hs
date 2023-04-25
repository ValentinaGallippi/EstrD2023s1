module SetV2
        (Set, emptySet, addS, belongs, sizeS, removeS, unionS, setToList)
    where 

data Set a = S [a]  

emptySet :: Set a -- constante
emptySet = S []

addS :: Eq a => a -> Set a -> Set a -- const
addS x (S xs) = S (x:xs)

belongs :: Eq a => a -> Set a -> Bool -- lineal 
belongs x (S xs) = elem x xs

sizeS :: Eq a => Set a -> Int -- lineal
sizeS (S xs) = length (sinRepetidos xs) 

removeS :: Eq a => a -> Set a -> Set a -- constante
removeS x (S xs) = S (sacar x xs)

sacar :: Eq a => a -> [a] -> [a] --parcial | lineal
sacar _ []      = []
sacar x (x':xs) = if x==x' then sacar x xs
                            else x' : sacar x xs

unionS :: Eq a => Set a -> Set a -> Set a -- constante
unionS (S xs) (S ys) = S(xs ++ ys)


setToList :: Eq a => Set a -> [a] -- constante
setToList (S xs) = sinRepetidos xs 

sinRepetidos :: Eq a => [a] -> [a] -- lineal
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x (sinRepetidos xs) then sinRepetidos xs 
                                                  else x : sinRepetidos xs 





