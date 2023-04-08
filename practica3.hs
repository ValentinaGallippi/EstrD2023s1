data Color = Azul | Rojo deriving Show

data Celda = Bolita Color Celda | CeldaVacia deriving Show 


-- Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
-- Implementar las siguientes funciones sobre celdas:
-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas color CeldaVacia       =  0
nroBolitas color (Bolita c celda) = unoSiCeroSiNo (esDeColor color c) + nroBolitas  color celda

esDeColor :: Color -> Color -> Bool
esDeColor Rojo Rojo = True
esDeColor Azul Azul = True
esDeColor _    _    = False 

unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo True  = 1
unoSiCeroSiNo False = 0

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner color CeldaVacia = Bolita color CeldaVacia
poner color celda = Bolita color celda
 
-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita color celda) =  if esDeColor color c then celda 
                                                     else (Bolita color (sacar c  celda ))


-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c celda = celda 
ponerN n c celda = Bolita c  (ponerN (n-1) c celda)

-- celda = Bolita Azul (Bolita Roja CeldaVacia)
-- c = Rojo
-- n = 2
-- ponerN (n-1) c celda = Bolita Roja (Bolita Azul (Bolita Roja CeldaVacia))
-- resultado final = Bolita Roja ((recursion))

--1.2 

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

--Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin                    = False
hayTesoro (Nada camino)          = hayTesoro camino 
hayTesoro (Cofre objetos camino) = hayTesorosEn objetos  || hayTesoro camino   

hayTesorosEn :: [Objeto] -> Bool 
hayTesorosEn [] = False 
hayTesorosEn (x:xs) =  esUnTesoro x  || hayTesorosEn xs 

esUnTesoro :: Objeto -> Bool 
esUnTesoro Tesoro = True
esUnTesoro _      = False

-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                    = error "no hay cofre" 
pasosHastaTesoro (Cofre objetos camino) = if hayTesorosEn objetos then 0  
                                          else 1 + pasosHastaTesoro camino
pasosHastaTesoro (Nada camino)          = 1 + pasosHastaTesoro camino 


-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 camino = hayTesoroJustoAca camino 
hayTesoroEn n camino = puedeAvanzar camino && hayTesoroEn (n-1) (avanzar camino)

hayTesoroJustoAca :: Camino -> Bool
hayTesoroJustoAca (Cofre objetos camino) = hayTesorosEn objetos 
hayTesoroJustoAca _ = False 

puedeAvanzar :: Camino -> Bool
puedeAvanzar Fin = False
puedeAvanzar _   = True

avanzar :: Camino -> Camino
avanzar (Cofre obj camino) = camino
avanzar (Nada camino)      = camino
avanzar Fin                = error "No se puede avanzar"

--Indica si hay al menos “n” tesoros en el camino. 
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = cantidadDeTesorosEnCamino camino == n 

cantidadDeTesorosEnCamino :: Camino -> Int 
cantidadDeTesorosEnCamino Fin                = 0 
cantidadDeTesorosEnCamino (Cofre obj camino) = cantidadDeTesorosEn obj + cantidadDeTesorosEnCamino camino 
cantidadDeTesorosEnCamino (Nada camino)      = cantidadDeTesorosEnCamino camino 

cantidadDeTesorosEn :: [Objeto] -> Int
cantidadDeTesorosEn [] = 0 
cantidadDeTesorosEn (o:os) = unoSiCeroSiNo (esUnTesoro o) + cantidadDeTesorosEn os 

-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incluidos tanto 3 como 5 en el resultado
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ 0 camino  = error "m tiene que ser mayor a n" 
cantTesorosEntre n m camino = cantidadDeTesorosEn_Hasta(avanzarN n camino) (m-n)

avanzarN :: Int -> Camino -> Camino
avanzarN 0 camino  = camino 
avanzarN n camino  = avanzarN (n-1) (avanzar camino) 

cantidadDeTesorosEn_Hasta :: Camino -> Int -> Int 
cantidadDeTesorosEn_Hasta camino 0 = cantidadDeTesorosAca camino
cantidadDeTesorosEn_Hasta Fin _    = error "m es mayor a la longitud del camino" 
cantidadDeTesorosEn_Hasta camino n = cantidadDeTesorosAca camino + cantidadDeTesorosEn_Hasta (avanzar camino) (n-1)

cantidadDeTesorosAca :: Camino -> Int 
cantidadDeTesorosAca (Cofre obj _) = cantidadDeTesorosEn obj 
cantidadDeTesorosAca _ = 0 



-- alMenosNTesoros 2 (Cofre [Cacharro,Cacharro] (Nada (Cofre [Tesoro,Tesoro] Fin)))


--2.1. Árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
-- en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT n t1 t2) = 1 + sizeT t1 + sizeT t2
-- sizeT (NodeT 5 EmptyT (NodeT 3 EmptyT EmptyT))

-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2)) 

-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
-- árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (NodeT y t1 t2) = x == y || perteneceT x t1 || perteneceT x t2

-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
-- iguales a e.

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT = 0
aparicionesT x (NodeT y t1 t2) = unoSiCeroSiNo (x==y)  + aparicionesT x t1 + aparicionesT x t2 

-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x t1 t2) = x : leaves t1 ++ leaves t2

-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
-- de niveles del árbol1
-- . La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + max (sizeT t1) (sizeT t2)

-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
-- en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1)) 

-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2

-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
-- nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
-- distancia de la raiz a uno de sus hijos es 1.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN n (NodeT x t1 t2) = if  n==0 then [x] else levelN (n-1) t1 ++ levelN  (n-1) t2  



-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
-- dicho árbol.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) =  [x] :  unirPorOrden (listPerLevel t1) (listPerLevel t2)

unirPorOrden :: [[a]] -> [[a]] -> [[a]]
unirPorOrden [] ys =  ys
unirPorOrden xs [] = xs
unirPorOrden (x:xs) (y:ys) = (x++y) : unirPorOrden xs ys 

-- unirPorOrden (xs:xss) (ys:yss) = [xs++ys] ++ unirPorOrden xs ys 

-- [ [2], [3]] ^[[2] [3]]
-- [[2,2], [3,3]]

-- [[2], [3], [2], [3]]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]                                 
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = if sizeT t1 >  sizeT t2 then x : leaves t1
                                                          else x : leaves t2

-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos  EmptyT = []
todosLosCaminos (NodeT x t1 t2) = caminosConRaiz x (todosLosCaminos t1 ++ todosLosCaminos t2)

caminosConRaiz :: a -> [[a]] -> [[a]]
caminosConRaiz x [] = []
caminosConRaiz x (xs:xss) = (x:xs)  :  caminosConRaiz x xss

 
-- x = 1 
-- todosLosCaminos t1 = [[2,5],[2,6]]
-- todosLosCaminos t2 = [[3,4]]
-- todosLosCaminos arbolito2 = [[1,2,5],[1,2,6],[1,3,4]]
arbolito2 :: Tree Int 
arbolito2 = NodeT 1 (NodeT 2 (NodeT 5 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT)) (NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT)

arbolito :: Tree Int 
arbolito = NodeT 1 (NodeT 2 EmptyT (NodeT 4 EmptyT (NodeT 8 EmptyT EmptyT))) (NodeT 3 (NodeT 5 EmptyT EmptyT) (NodeT 6 EmptyT EmptyT))

--2.2. Expresiones Aritméticas
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA  deriving Show

-- Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA -> Int
eval (Valor n)    = n 
eval (Sum e1 e2)  = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval  (Neg e1)    = - eval e1

-- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
-- notación matemática convencional):
-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d) - (- x) = x
simplificar :: ExpA -> ExpA
simplificar (Valor n)   = n
simplificar (Sum e1 e2) = simplificarSuma (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2)= simplificarMul (simplificar e1) (simplificar e2)
simplificar (Neg e1)    = simplificarNeg (simplificar e1)

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg e1) = e1



simplificarMul :: ExpA -> ExpA -> ExpA
simplificarMul e (Valor 1) = e
simplificarMul (Valor 1) e = e
simplificarMul e (Valor 0) = (Valor 0)
simplificarMul (Valor 0) e = (Valor 0)
simplificarMul e1 e2       = Prod e1 e2 

simplificarSuma :: ExpA -> ExpA -> ExpA 
simplificarSuma e (Valor 0)= e
simplificarSuma (Valor 0) e=  e
simplificarSuma e e1 = Sum e1 e2

