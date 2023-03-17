sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar x y = x + y 

-- divisionYResto = undefined

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y then x else y

{-ejercicio 2
De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))-}

-- {sumar (maxDelPar (divisionYResto 20 2 , sucesor 5)) 0  }

data Dir = Norte | Este | Sur | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto Este = Oeste
opuesto Norte = Sur
opuesto Oeste = Este
opuesto Sur = Norte

iguales :: Dir -> Dir -> Bool
iguales Este Este   = True
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Oeste Oeste = True
iguales _     _     = False

{-Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
total o parcial? Por qué?-}


siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = error "no hay siguiente de oeste"


data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo  deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia =  (primerDiaDeLaSemana, ultimoDiaDeLaSemana)

primerDiaDeLaSemana = Lunes
ultimoDiaDeLaSemana = Domingo

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

{-c) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).-}

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True 
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues Lunes Domingo  = True
vieneDespues _     _        = False 


estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False --si aca uso primerDiaDeLaSemana y ultimoDiaDeLaSemana no funciona, por qué? 
estaEnElMedio Domingo = False
estaEnElMedio _       = True

negar :: Bool -> Bool
negar True = False
negar False = True

{-b) implica :: Bool -> Bool -> Bool
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True. Esta función debe ser tal que implica False (error "Mal") devuelva
True-}

implica :: Bool -> Bool -> Bool
implica True  False = False
implica _     _     = True

{-yTambien :: Bool -> Bool -> Bool
Dados dos booleanos si ambos son True devuelve True, sino devuelve False. Esta
función debe ser tal que yTambien False (error "Mal") devuelva False.
En Haskell ya está denida como \&\&.
-}

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _     _   = False

oBien :: Bool -> Bool -> Bool
oBien True True = False
oBien True _ = True
oBien _ True = True
oBien _   _  = False