module SetV1 
            (Set, emptySet, addS, belongs, sizeS, removeS, unionS, setToList) 
where 

data Set a = S [a] 

-- Inv.Rep.:  tal que [a] 
--         *  no tiene repetidos  

-- Invalido : [1,1,3,4,5] -> tiene repetidos
-- Valido   : [1,2,3,4,5] 

emptySet :: Set a -- constante
emptySet = S []

addS :: Eq a => a -> Set a -> Set a -- preguntar
addS x (S xs) = S (agregarSinRepetir x xs)

agregarSinRepetir ::Eq a => a -> [a] -> [a]
-- PRECONDICION : la lista no tiene elementos repetidos 
agregarSinRepetir x [] = [x]
agregarSinRepetir x (x':xs) = 
   if x == x' 
    then x' : xs
    else x' : agregarSinRepetir
 x xs 

belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = elem x xs

sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs 

removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs) = S (sacar x xs)

sacar :: Eq a => a -> [a] -> [a] --parcial 
--PRECOND: la lista no tiene elementos repetidos y existe el elemento que tengo qeu sacar. 
sacar _ []      = []
sacar x (x':xs) = if x==x' then xs
                            else x' : sacar x xs

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S(unirSinRepetir xs ys)

unirSinRepetir :: Eq a => [a] -> [a] -> [a]
-- PRECONDICION: las listas no tienen elementos repetidos
unirSinRepetir [] ys     = ys
unirSinRepetir (x:xs) ys = agregarSinRepetir x (unirSinRepetir xs ys )

setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs 





