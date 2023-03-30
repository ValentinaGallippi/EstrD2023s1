--1. Recursión sobre listas
--1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns
--2 
longitud :: [a] -> Int
longitud []     = 0 
longitud (x:xs) = 1 + longitud xs  
--3
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) =  n+1  : sucesores ns

--4 
conjuncion :: [Bool] -> Bool
conjuncion []     =  True 
conjuncion (b:bs) =   b  && conjuncion bs

--5 
disyuncion :: [Bool] -> Bool
disyuncion []     =  False 
disyuncion (b:bs) =   b  || disyuncion bs

--6
aplanar :: [[a]] -> [a]
--precond: las listas adentro de la lista tienen que ser del mismo tipo 
aplanar [] = []
aplanar (x:xs) =  x ++ aplanar xs 

--7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _  []     = False
pertenece e  (x:xs) = e==x ||  pertenece e xs 

--8 
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSiNo (e==x) + apariciones e xs


unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo condicion = if condicion then 1
                          else 0 

-- BIBLIOTECA:
-- function singular_Si_(elemento, condición) {
--  /* PROPÓSITO: Describir una lista según el valor de la
--  condición dada. Si es verdadera, describe la lista singular
--  con **elemento**. Si no, describe la lista vacía.
--  PRECONDICIONES: Ninguna.
--  PARÁMETROS:
--  * elemento: De un tipo cualquiera.
--  * condición: Booleano.
--  TIPO: Lista del tipo de **elemento**.
--  */
--  return (choose [elemento] when (condición)
--  [] otherwise)
-- }
--9
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []    = []
losMenoresA x (n:ns) = singularSi n (n<x) ++ losMenoresA x ns 

singularSi :: a -> Bool -> [a]
singularSi x condicion = if condicion then [x]
                         else []

singularSi' :: a -> Bool -> [a]
singularSi' x True  = [x]
singularSi' _ False = []
--10
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
--de n elementos
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     =  []
lasDeLongitudMayorA n (x:xs) = singularSi x (longitud x > n  ) ++ lasDeLongitudMayorA n xs

--11 
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al nal de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     a = [a]
agregarAlFinal (x:xs) y = x : (agregarAlFinal xs y)


--12
agregar :: [a] -> [a] -> [a]
agregar []     a      = a
agregar a      []     = a
agregar (x:xs) ys =   x :  (agregar xs ys)

--13
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa  xs) x 

--14 
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos _       []     = []
zipMaximos []      _      = []
zipMaximos (x:xs) (y:ys)  = maximoEntre x y : zipMaximos xs ys 


maximoEntre :: Int -> Int -> Int
maximoEntre n m = if n > m then n
                           else m 

--15.
elMinimo' :: Ord a=> [a] -> a
elMinimo' []     = error "esta mal"
elMinimo' (x:[]) = x
elMinimo' (x:xs) = min x (elMinimo' xs) 


elMinimo :: Ord a=> [a] -> a
elMinimo []     = error "esta mal"
elMinimo (x:xs) = if esVacia xs  then x
                                 else min x (elMinimo xs)

esVacia :: [a] -> Bool
esVacia [] = True
esVacia _ = False

-- elMinimo (x:xs) =  ...x ...elMinimo xs

-- [2,6,7,1]
-- x=2
-- xs=6,7,1
-- elMinimo xs = 1

-- 2. Recursión sobre números
--1 
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial n-1


--2 
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)


--3 
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n x = x : (repetir (n-1) x)

--4
-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros  0 _      = []
losPrimeros  _ []     = []
losPrimeros  n (x:xs) = x : losPrimeros (n-1) xs 

--5
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 (y:ys)= (y:ys)
sinLosPrimeros _ []  = []
sinLosPrimeros n (x:xs) =  sinLosPrimeros (n-1) xs 

--3. Registros
data Persona = P String Int  deriving Show
              -- Nombre Edad

--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if edad x > n 
                    then x : (mayoresA n xs)
                    else (mayoresA n xs)

edad :: Persona -> Int
edad (P n e) = e

--Dada una lista de personas devuelve el promedio de edad entre esas personas. precondicion: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad []     = error "la lista debe tener al menos 1 elemento"
promedioEdad xs =  div (sumatoria (edades xs))  (longitud xs)

edades :: [Persona] -> [Int]
edades [] = []
edades (x:xs) = edad x : (edades xs)

-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
-- lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "la lista debe tener al menos 1 elemento"
elMasViejo (x:[])= x 
elMasViejo (x:xs) = laQueEsMayor x (elMasViejo xs)

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor x y = if  esMayorQueLaOtra x y 
                                then  x
                                else  y

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra x  y = edad x > edad y   

--3
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon       = ConsPokemon TipoDePokemon Int
data Entrenador    = ConsEntrenador String [Pokemon]

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador _ pokemones) = pokemones 

cantPokemon :: Entrenador -> Int
cantPokemon e = longitud (pokemonesDe e )

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t e = longitud (pokemonesDeTipo t (pokemonesDe e))

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipo _ [] = []
pokemonesDeTipo t (x:xs) = if esDeTipo t (tipoPokemon x) then x : pokemonesDeTipo t xs 
                           else pokemonesDeTipo t xs 

esDeTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeTipo Agua Agua     = True
esDeTipo Planta Planta = True
esDeTipo Fuego Fuego   = True
esDeTipo _           _ = False

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (ConsPokemon tipo _ ) = tipo 

-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_  t e1 e2 =  longitud (losQueSonSuperiores (pokemonesDeTipo t (pokemonesDe e1))  (pokemonesDe e2))

losQueSonSuperiores :: [Pokemon] -> [Pokemon] -> [Pokemon]
losQueSonSuperiores [] _ = []
losQueSonSuperiores _ [] = []
losQueSonSuperiores (x:xs) ps = if esSuperiorATodos x ps then x : losQueSonSuperiores xs ps
                                                    else losQueSonSuperiores xs ps

esSuperiorATodos :: Pokemon -> [Pokemon] -> Bool
esSuperiorATodos _ [] = False 
esSuperiorATodos p (x:xs) =  esTipoSuperior (tipoPokemon p) (tipoPokemon x) && esSuperiorATodos p xs 

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego   = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua  = True
esTipoSuperior _      _     = False


--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = hayUnoDeTipo Agua   (pokemonesDe e) &&
                     hayUnoDeTipo Fuego  (pokemonesDe e) &&
                     hayUnoDeTipo Planta (pokemonesDe e) 

hayUnoDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayUnoDeTipo _    []     = False
hayUnoDeTipo tipo (p:ps) = esDeTipo tipo (tipoPokemon p) || hayUnoDeTipo tipo ps 

--3
-- El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
-- de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
-- una lista de personas con diferente rol. La denición es la siguiente:

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) = proyectosDe roles

proyectosDe :: [Rol] -> [Proyecto] 
proyectosDe [] = []
proyectosDe (r:rs) = proyecto r : proyectosDe rs

proyecto :: Rol -> Proyecto
proyecto (Management s  p ) = p 
proyecto (Developer  s1 p1 )= p1 



empresa = ConsEmpresa [(Developer Junior (ConsProyecto "google")), (Management Junior (ConsProyecto "google")), (Developer SemiSenior (ConsProyecto "Pinterest"))]

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
-- además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) proyectos = pertencenALosProyectos proyectos (devSeniorsDe roles)

devSeniorsDe :: [Rol] -> [Rol] 
devSeniorsDe [] = []
devSeniorsDe (r:rs) = if esDevSenior r then r : devSeniorsDe rs
                                       else devSeniorsDe rs

esDevSenior :: Rol -> Bool 
esDevSenior (Developer Senior _ ) = True
esDevSenior _                     = False 

pertencenALosProyectos :: [Proyecto] -> [Rol] -> Int
pertencenALosProyectos [] _  = 0
pertencenALosProyectos _  [] = 0
pertencenALosProyectos ps (r:rs) = unoSiCeroSiNo (pertenceALosProyectos ps r) + pertencenALosProyectos ps rs   

pertenceALosProyectos :: [Proyecto] -> Rol -> Bool
pertenceALosProyectos [] _ = False
pertenceALosProyectos (p:ps) rol = esElMismoProyecto (proyecto rol)  p  && pertenceALosProyectos ps rol 

esElMismoProyecto :: Proyecto -> Proyecto -> Bool
esElMismoProyecto p1 p2 = nombreProyecto p1 == nombreProyecto p2 

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto nombre) = nombre 

--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa roles) = pertencenALosProyectos'  ps roles 


pertencenALosProyectos' :: [Proyecto] -> [Rol] -> Int
pertencenALosProyectos' [] _  = 0
pertencenALosProyectos' _  [] = 0
pertencenALosProyectos' ps (r:rs) = unoSiCeroSiNo (pertenceALosProyectos' ps r) + pertencenALosProyectos' ps rs   

pertenceALosProyectos':: [Proyecto] -> Rol -> Bool
pertenceALosProyectos' [] _ = False
pertenceALosProyectos' (p:ps) rol = esElMismoProyecto (proyecto rol)  p  || pertenceALosProyectos' ps rol


-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
-- cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto = undefined 

