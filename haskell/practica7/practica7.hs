-- Ejercicio 1 
heapSort :: Ord a => [a] -> [a] -- O(n log n)
heapSort xs = ordenado (listaAPQ xs )

ordenado ::  Ord a => PriorityQueue a -> [a] -- O(n log n)
ordenado pq = if isEmptyPQ pq then []
                              else findMinPQ pq : ordenado (deleteMinPQ pq)

listaAPQ ::  Ord a =>  [a] -> PriorityQueue a -- O(n log n)
listaAPQ []     =  emptyPQ 
listaAPQ (x:xs) =  insertPQ x  (listaAPQ xs)

--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST x EmptyT          = False 
belongsBST x (NodeT y ti td) = 
    if (x==y)      then True
     else if (x<y) then buscarBST x ti
                   else buscarBST x td



insertBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST inserta un elemento en el árbol.
--Costo: O(log N)
insertBST x EmptyT          = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = 
    if (x == y)    then NodeT x ti td
     else if (x>y) then NodeT y ti (insertBST x td)
                   else NodeT y (insertBST x ti) td
 

--Propósito: dado un BST borra un elemento en el árbol.
--Costo: O(log N)
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT          = EmptyT 
deleteBST x (NodeT y ti td) =
    if      (x==y) then rearmarBST ti td
    else if (x>y)  then deleteBST x td 
                   else deleteBST x ti

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a 
rearmarBST ti td = NodeT (maxDe ti) (sinElmax ti) td  

maxDe :: Tree a -> a
-- PRECONDICION : t no es vacio. 
maxDe (NodeT x ti EmptyT) = x
maxDe (NodeT x ti td)     = maxDe td 

sinElmax :: Tree a -> Tree a  
-- PRECOND: el arbol no esta vacio. 
sinElmax (NodeT x ti EmptyT) = ti  
sinElmax (NodeT x ti td)     = NodeT x ti (sinElmax td)

--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--Costo: O(log N)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST t = ((min t),(sinMin t))

minT :: Tree a -> a 
-- PRECOND: el arbol no esta vacio. 
minT (NodeT x EmptyT td) = x
minT (NodeT x ti     td) = minT ti

sinElmin :: Tree a -> Tree a  
-- PRECOND: el arbol no esta vacio. 
sinElmin (NodeT x EmptyT td) = td  
sinElmin (NodeT x ti     td) = NodeT x (sinElMin ti) td

--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--Costo: O(log N)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST t = ((max t),(sinElmax t))

--Propósito: indica si el árbol cumple con los invariantes de BST.
esBST :: Ord a => Tree a -> Bool
esBST EmptyT           = True
esBST (NodeT x ti td)  = esMayorA x ti && esMenor x td && (esBST ti) && (esBST td)

esMayorA :: Ord a => a -> Tree a -> Bool
esMayorA x EmptyT          = True
esMayorA x (NodeT y ti td) = x > y 

esMenor :: Ord a => a -> Tree a -> Bool
esMenor x EmptyT          = True
esMenor x (NodeT y ti td) = x < y 

-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N)
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA x EmptyT = Nothing
elMaximoMenorA x (NodeT y ti td) = 
    if (x<=y) then elMaximoMenorA x ti
              else  case elMaximoMenorA x td of 
                           Nothing -> Just y
                           Just v -> Just v 

-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
-- elemento dado.
-- Costo: O(log N)
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA x EmptyT = Nothing
elMinimoMayorA x (NodeT y ti td) = 
    if (x>=y) then elMinimoMayorA x td
              else  case elMinimoMayorA x ti of 
                           Nothing -> Just y
                           Just v -> Just v 

--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
--Costo: O(N2) -- 
balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT x t1 t2) =  abs (heightT t1 - heightT t2) <= 1 && balanceado t1 && balanceado t2 


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

import Empresa
-- (ConsE, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, borrarEmpleado)
-- Propósito: construye una empresa con la información de empleados dada. Los sectores no
-- tienen empleados.
-- Costo: calcular.
comenzarCon :: [SectorId] -> [CUIL] -> Empresa -- FEOOOOOOOOOO PUTOOOOO TODO JUNTOD HACE SUBTAERAS PARA QEU CARAJO CURSASTE INTRO GORDA PUTA
comenzarCon []       c      = consEmpresa
comenzarCon (id:ids) []     = consEmpresa
comenzarCon (id:ids) (c:cs) = agregarEmpleado [] c (agregarASector id  (comenzarCon ids cs)) 

-- Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
-- Costo: calcular.
recorteDePersonal :: Empresa -> Empresa
--PRECOND: tiene que haber al menos 1 empleado. 
recorteDePersonal empresa = let empleados = todosLosCUIL empresa 
                                cantidadABorrar = div (lenght empleados) 2 in
    borrarEmpleados cantidadABorrar empleados empresa

borrarEmpleados :: Int -> [CUIL] -> Empresa -> Empresa
borrarEmpleados 0 _ empresa = empresa
borrarEmpleados n (c:cs) empresa = borrarEmpleado c (borrarEmpleados (n-1) cs empresa)

{-COSTO:    
    recorteDePersonal:
    todosLosCUIL + lenght + borrarEmpleados 
    E            + E      +  E* Log E
    E + E*Log E 
    E * (Log E )
    borrarEmpleados:
    E * Log E 
    -} 

-- Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
-- Costo: calcular.
convertirEnComodin :: CUIL -> Empresa -> Empresa --preguntar si es asi o como el de santi. 
convertirEnComodin c empresa = agregarEmpleado c (todosLosSectores empresa) empresa


-- Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
-- Costo: calcular.
esComodin :: CUIL -> Empresa -> Bool
esComodin c empresa = estaEnTodosLosSectores c (todosLosSectores empresa) empresa

estaEnTodosLosSectores ::CUIL -> [SectorId] -> Empresa -> Bool
estaEnTodosLosSectores c [] e       = False
estaEnTodosLosSectores c (id:ids) e = esEmplado c (empleadosDelSector id e) && estaEnTodosLosSectores