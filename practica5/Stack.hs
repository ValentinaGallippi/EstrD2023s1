module Stack 
            (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where 

data Stack a = S [a]

emptyS :: Stack a
-- Crea una pila vacía.
emptyS = S []

isEmptyS :: Stack a -> Bool
-- Dada una pila indica si está vacía.
isEmptyS (S xs) = null xs

push :: a -> Stack a -> Stack a
-- Dados un elemento y una pila, agrega el elemento a la pila.
push x (S xs) = S (x:xs)

top :: Stack a -> a
-- Dada un pila devuelve el elemento del tope de la pila.
top (S xs) = head xs

pop :: Stack a -> Stack a
-- Dada una pila devuelve la pila sin el primer elemento.
pop (S xs) = S (tail xs)

lenS :: Stack a -> Int
-- Dada la cantidad de elementos en la pila.
-- Costo: constante.
lenS (S xs) = length xs

