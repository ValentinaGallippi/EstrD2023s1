--2. Números enteros
--1a)
sucesor :: Int -> Int
sucesor x = x + 1
--b)
sumar :: Int -> Int -> Int
sumar x y = x + y 
--c)
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)
--d)
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y then x else y
{-ejercicio 2
De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))-}
-- sumar (maxDelPar (5,1))  (maxDelPar(divisionYResto 10 (sucesor 1)))
-- sucesor (sumar (maxDelPar (5,1)) (maxDelPar (divisionYResto 8 2)))
-- maxDelPar (sumar 5 (sucesor 4),  maxDelPar (divisionYResto 18 9))
-- maxDelPar (divisionYResto (sucesor 19)  (maxDelPar(sumar 1 1 , 0)) -> me devuelve una tupla (10,0))

--3. Tipos enumerativos
--1 
data Dir = Norte | Este | Sur | Oeste deriving Show
--a)
opuesto :: Dir -> Dir
opuesto Este = Oeste
opuesto Norte = Sur
opuesto Oeste = Este
opuesto Sur = Norte
--b)
iguales :: Dir -> Dir -> Bool
iguales Este Este   = True
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Oeste Oeste = True
iguales _     _     = False
--c
{-Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
total o parcial? Por qué?-}

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = error "no hay siguiente de oeste"

--2 
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo  deriving Show
--a)
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia =  (primerDiaDeLaSemana, ultimoDiaDeLaSemana)

primerDiaDeLaSemana = Lunes
ultimoDiaDeLaSemana = Domingo
--b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False
--c)
{- vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).-}

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues x Lunes     = numeroDelDia x > 1
vieneDespues a Martes    = numeroDelDia a > 2
vieneDespues b Miercoles = numeroDelDia b > 3
vieneDespues c Jueves    = numeroDelDia c > 4
vieneDespues d Viernes   = numeroDelDia d > 5
vieneDespues e Sabado    = numeroDelDia e > 6
vieneDespues _ Domingo   = False

 

numeroDelDia :: DiaDeSemana -> Int
numeroDelDia Lunes     = 1
numeroDelDia Martes    = 2
numeroDelDia Miercoles = 3
numeroDelDia Jueves    = 4
numeroDelDia Viernes   = 5
numeroDelDia Sabado    = 6
numeroDelDia Domingo   = 7

--d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False  
estaEnElMedio Domingo = False
estaEnElMedio _       = True
--3
--a)
negar :: Bool -> Bool
negar True = False
negar False = True
--b)
{- implica :: Bool -> Bool -> Bool
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True. Esta función debe ser tal que implica False (error "Mal") devuelva
True-}

implica :: Bool -> Bool -> Bool
implica True  x     = x
implica _     _     = True

--c)
{-yTambien :: Bool -> Bool -> Bool
Dados dos booleanos si ambos son True devuelve True, sino devuelve False. Esta
función debe ser tal que yTambien False (error "Mal") devuelva False.
En Haskell ya está denida como \&\&.
-}

yTambien :: Bool -> Bool -> Bool
yTambien True x = x
yTambien  _   _ = False

--d)
oBien :: Bool -> Bool -> Bool
oBien False b = b
oBien True _ = True


--4. Registros
--1.
data Persona = P String Int  deriving Show

nombre :: Persona -> String
nombre (P n e) = n 

valen :: Persona
valen = P "Valentina" 19

flor :: Persona
flor = P "Florencia" 33 


edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P n e) = (P nuevoNombre e )

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra x  y = edad x > edad y       

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P nomb ed) (P n e) = if ed > e 
                                   then (P nomb ed)
                                   else (P n e)
--2
data Pokemon = Po TipoDePokemon Int deriving Show
                --Tipo          Porcentaje de bateria 
data TipoDePokemon = Agua | Fuego | Planta deriving Show

data Entrenador = E String Pokemon Pokemon deriving Show
                --  Nombre pokemon1 pokemon2
--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso
superaA :: Pokemon -> Pokemon -> Bool
superaA (Po t1 _) (Po t2 _)   = esMismoTipo t1 t2

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua     = True
esMismoTipo Fuego Fuego   = True
esMismoTipo Planta Planta = True

pokemon1 = (Po Agua 4)
pokemon2 = (Po Fuego 6)
pokemon3 = (Po Planta 66)
pokemon4 = (Po Planta 777)

entrenador1 = (E "Claudio" pokemon1 pokemon2)

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (Po tipo _ ) = tipo 


--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (E _ p1 p2) = unoSiCeroSiNo (esDeTipo (tipoPokemon p1) tipo) + unoSiCeroSiNo (esDeTipo (tipoPokemon p2) tipo)

esDeTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeTipo Agua Agua     = True
esDeTipo Planta Planta = True
esDeTipo Fuego Fuego   = True
esDeTipo _           _ = False

unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo condicion = if condicion then 1
                          else 0 

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ p1 p2), (E _ po1 po2)) = [p1,p2,po1,po2]

--5. Funciones polimórficas
--1
--a)
loMismo :: a -> a
loMismo x = x

--b)
siempreSiete :: a -> Int
siempreSiete x = 7

--c)
swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

--6. Pattern matching sobre listas
--2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

--3 
elPrimero :: [a] -> a
elPrimero (x:_) = x

--4
sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs
--5
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)