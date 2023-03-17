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
