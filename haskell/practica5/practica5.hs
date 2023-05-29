import SetV2
-- (emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

import QueueV3
-- (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

import Stack 
--  (Stack, emptyS, isEmptyS, push, top, pop, lenS)

--------------------------------------------------------------------------------------------------------------
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = singularSi x (belongs x s) ++ losQuePertenecen xs s

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi _ False = []


-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listaAConjunto xs)

listaAConjunto :: [a] -> Set a 
listaAConjunto [] = emptyS
listaAConjunto (x:xs) = addS x (listaAConjunto xs)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptySet
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))


tree1 = NodeT (addS 6(addS 6(addS 5 (emptySet)))) (NodeT (addS 6(addS 6(addS 5 (emptySet)))) EmptyT EmptyT) EmptyT

-------------------------------------------------------------------------------------------------------------------------------------------

-- Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q = if not (isEmptyQ q) then 1 + lengthQ (dequeue q)
                                else 0 


-- Dada una cola devuelve la lista con los mismos elementos,
-- donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q then [] 
                              else firstQ q : queueToList (dequeue q) 

  
-- Inserta todos los elementos de la segunda cola en la primera
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2 then q1 
                              else unionQ (enqueue  (firstQ q2) q1) (dequeue q2)


------------------------------------------------------------------------------------------------

-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s then []
                            else  top s : desapilar (pop s)

insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).                      
insertarEnPos 0 e s = push e s 
insertarEnPos n e s = if isEmptyS s 
                       then push e s

                       else push (top s) (insertarEnPos (n-1) e (pop s))


