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
sinElmax (NodeT x ti EmptyT) = ti  
sinElmax (NodeT x ti td)     = NodeT x ti (sinElmax td)

--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--Costo: O(log N)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST = undefined 


--splitMaxBST :: Ord a => Tree a -> (a, Tree a)
----Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
----Costo: O(log N)
--
--esBST :: Tree a -> Bool
----Propósito: indica si el árbol cumple con los invariantes de BST.
----Costo: O(N2
----)
--elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
----Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
----elemento dado.
----Costo: O(log N)
--elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
----Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
----elemento dado.
----Costo: O(log N)
--balanceado :: Tree a -> Bool
----Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
----nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
----Costo: O(N2)