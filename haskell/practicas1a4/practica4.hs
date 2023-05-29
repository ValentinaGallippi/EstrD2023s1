-- 1. Pizzas
data Pizza = Prepizza | Capa Ingrediente Pizza
                        deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
                    deriving Show


-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0 
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p 

-- Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is) 

-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) =   if esJamon ing then  sacarJamon p
                                           else (Capa ing (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False


-- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
-- particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza     = True
tieneSoloSalsaYQueso (Capa ing p) = esSalsaOQueso ing && tieneSoloSalsaYQueso p  


esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False 

-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarSiEs ing) (duplicarAceitunas p)

duplicarSiEs :: Ingrediente -> Ingrediente 
duplicarSiEs (Aceitunas n) = (Aceitunas (n*2))
duplicarSiEs x             = x


-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
-- ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p , p) : cantCapasPorPizza ps 

pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) 
              (Capa Queso (Capa Salsa Prepizza))

----------------------------------------------------------------------------------------------------------
--2. Mapa de tesoros (con bifurcaciones)

data Dir = Izq | Der                                 deriving Show  
data Objeto = Tesoro | Chatarra                      deriving Show    
data Cofre = Cofre [Objeto]                          deriving Show         
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa  deriving Show        

-- Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre) = hayTesoroEnCofre cofre 
hayTesoro (Bifurcacion cofre m1 m2) = hayTesoroEnCofre cofre ||  hayTesoro m1 ||  hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool 
hayTesoroEnCofre (Cofre objs) = hayTesorosEn objs

hayTesorosEn :: [Objeto] -> Bool 
hayTesorosEn [] = False 
hayTesorosEn (x:xs) =  esUnTesoro x  || hayTesorosEn xs 

esUnTesoro :: Objeto -> Bool 
esUnTesoro Tesoro = True
esUnTesoro _      = False


-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
-- lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] mapa                          =  hayTesoro mapa 
hayTesoroEn (d:ds) (Fin cofre)               =  error "hay mas direcciones que mapa "
hayTesoroEn (d:ds) (Bifurcacion cofre m1 m2) = if esIzq d then hayTesoroEn ds m1 
                                                          else hayTesoroEn ds  m2 

esIzq Izq = True
esIzq _   = False 

mapa1 = Bifurcacion cofre2 (Bifurcacion cofre2 (Bifurcacion cofre2 (Fin cofre2) (Fin cofre2)) (Fin cofre2)) (Fin cofre1)

cofre1 = Cofre [Tesoro,Tesoro,Chatarra]

cofre2 = Cofre [Chatarra]


-- Indica el camino al tesoro. 
caminoAlTesoro :: Mapa -> [Dir]
-- Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro (Bifurcacion cofre m1 m2) = if hayTesoroEnCofre cofre  then [] else
                                           if hayTesoro m1 then Izq : caminoAlTesoro m1 
                                           else Der : caminoAlTesoro m2 

-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _ ) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if largoDe m1 >= largoDe m2 
                                               then Izq : caminoDeLaRamaMasLarga m1 
                                               else Der : caminoDeLaRamaMasLarga m2


largoDe :: Mapa -> Int 
largoDe (Fin _ ) = 0
largoDe (Bifurcacion _ m1 m2) =  1 + largoDe m1 + largoDe m2

-- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cofre) = [objetos cofre]
tesorosPorNivel (Bifurcacion cofre m1 m2) =  objetos cofre : unirPorNivel (tesorosPorNivel m1) (tesorosPorNivel m2)

objetos :: Cofre -> [Objeto] 
objetos (Cofre obs) = obs 

unirPorNivel :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
unirPorNivel [] objetos          = objetos
unirPorNivel obj []              = obj
unirPorNivel (o:objs) (o2:objs2) = ( o ++ o2) : unirPorNivel objs objs2

-- Devuelve todos lo caminos en el mapa
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _ ) = []
todosLosCaminos (Bifurcacion _ m1 m2) =  conDirActual Izq (todosLosCaminos m1) ++  conDirActual  Der (todosLosCaminos m2)

conDirActual :: Dir -> [[Dir]] -> [[Dir]]
conDirActual d [] = [[d]]
conDirActual d (ds:dss) = (d : ds) : conDirActual d dss




-- 3. Nave Espacial
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]  deriving Show 

data Barril = Comida | Oxigeno | Torpedo | Combustible  deriving Show 

data Sector = S SectorId [Componente] [Tripulante]  deriving Show 

type SectorId = String  

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)  deriving Show 

data Nave = N (Tree Sector)  deriving Show 

 
nave0 = N (NodeT sector1 (NodeT sector2 EmptyT (NodeT sector1 EmptyT EmptyT)) EmptyT)

sector1 = S "31c" [LanzaTorpedos,(Motor 18), (Almacen [Comida,Oxigeno,Combustible])] ["valen"]

sector2 = S "rr43" [(Motor 32),(Almacen [Comida,Combustible]), (Almacen [Comida,Oxigeno,Combustible])] ["gallippi"]

--1 Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores (N t) = sectoresS t 

sectoresS :: Tree Sector -> [SectorId]
sectoresS EmptyT = []
sectoresS (NodeT s t1 t2) = idSector s : sectoresS t1 ++ sectoresS t2

idSector :: Sector -> SectorId 
idSector (S sI _ _) = sI 

-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = poderDePropulsionT t 

poderDePropulsionT :: Tree Sector -> Int 
poderDePropulsionT EmptyT          = 0
poderDePropulsionT (NodeT s t1 t2) = propulsion s + poderDePropulsionT t1 + poderDePropulsionT t2 

propulsion :: Sector -> Int
propulsion (S _ c _) = propulsionesC c 

propulsionesC :: [Componente] -> Int 
propulsionesC [] = 0
propulsionesC (c:cs) = valorSiEsMotor c + propulsionesC cs

valorSiEsMotor :: Componente -> Int
valorSiEsMotor (Motor n) = n
valorSiEsMotor  _        = 0 


-- Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t 

barrilesT :: Tree Sector -> [Barril] 
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesS s ++ barrilesT t1 ++ barrilesT t2

barrilesS :: Sector -> [Barril] 
barrilesS (S _ c _ ) = barrilesC c 

barrilesC :: [Componente] -> [Barril]
barrilesC [] = []
barrilesC (c:cs) = barrilSiAlmacen c ++ barrilesC cs 

barrilSiAlmacen :: Componente -> [Barril]
barrilSiAlmacen (Almacen b) = b 
barrilSiAlmacen _           = []

-- Propósito: Añade una lista de componentes a un sector de la nave.
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sI (N t) = N (agregarAS cs sI t)

agregarAS :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarAS cs sI EmptyT = EmptyT 
agregarAS cs sI (NodeT s t1 t2) = if sI == idSector s then agregarComponentes cs s t1 t2
                                 else NodeT s (agregarAS cs sI t1) (agregarAS cs sI t2)

agregarComponentes :: [Componente] -> Sector -> Tree Sector -> Tree Sector -> Tree Sector 
agregarComponentes cs s t1 t2 = NodeT (sectorConComponentes s cs ) t1 t2 

sectorConComponentes :: Sector -> [Componente] -> Sector 
sectorConComponentes (S sI cs t) comp  = (S sI (cs++comp) t)

-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tp sIs (N t) = N (tripulanteEnSectores tp sIs t)

tripulanteEnSectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
tripulanteEnSectores t sIs EmptyT = EmptyT
tripulanteEnSectores t sIs (NodeT s t1 t2) = (NodeT (sectorConTripulanteSi s t (estaEnLaLista s sIs))
                                               (tripulanteEnSectores  t sIs t1)
                                               (tripulanteEnSectores  t sIs t2))


sectorConTripulanteSi :: Sector -> Tripulante -> Bool -> Sector
sectorConTripulanteSi (S sI cs ts) t True = (S sI cs (t:ts) )
sectorConTripulanteSi  sector      _ False = sector

estaEnLaLista :: Sector -> [SectorId] -> Bool
estaEnLaLista s [] = False 
estaEnLaLista s (sc:scs) = idSector s == sc || estaEnLaLista s scs

-- Propósito: Devuelve los sectores en donde aparece un tripulante dado
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N ts) = sectoresConTripulante t ts 

sectoresConTripulante :: Tripulante -> Tree Sector ->  [SectorId]
sectoresConTripulante t EmptyT  = []
sectoresConTripulante t (NodeT s t1 t2) =  if (esTripulanteEn t s) then idSector s : (sectoresConTripulante t t1) ++ (sectoresConTripulante t t2)
                                            else (sectoresConTripulante t t1) ++ (sectoresConTripulante t t2)

esTripulanteEn :: Tripulante -> Sector -> Bool
esTripulanteEn t (S id cs ts ) = tripulanteEstaEn t ts 

tripulanteEstaEn :: Tripulante -> [Tripulante] -> Bool
tripulanteEstaEn t [] = False
tripulanteEstaEn t (tp:tps) = t == tp || tripulanteEstaEn t tps

----------------------------------------------------------------------------------------------
type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre

data Manada = M Lobo

--1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crías. 
manada1 = M (Cazador "h" [] (Cria "d")  (Cazador "b" [] (Cria "x") (Cria "d")  (Cria "d")) (Explorador "e" [] (Cria "x")  (Cazador "c" [] (Cria "x") (Cria "d") (Cazador "a" ["conejo", "liebre"] (Explorador "b" ["rioA","rioB"] (Cria "c") (Cria "d")) (Explorador "e" ["rioA" ,"rio1"] (Cria "e") (Cria "f")) (Cria "g" )))))


--2 
--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = cantidadDePresas lobo > cantidadDeCrias lobo

cantidadDePresas :: Lobo -> Int
cantidadDePresas (Cria _ )                      = 0 
cantidadDePresas (Explorador _ _ l1 l2)         = cantidadDePresas l1 + cantidadDePresas l2 
cantidadDePresas (Cazador    _ presas l1 l2 l3) = length presas + cantidadDePresas l1 + cantidadDePresas l2 + cantidadDePresas l3

cantidadDeCrias :: Lobo -> Int 
cantidadDeCrias (Cria _                      ) = 1 
cantidadDeCrias (Explorador _ _ l1 l2        ) = cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cazador    _ presas l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3

--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elLoboAlfa lobo

elLoboAlfa :: Lobo -> (Nombre, Int) 
elLoboAlfa (Cria nombre                       ) = (nombre,0)
elLoboAlfa (Explorador nombre _ l1 l2    )      = elegirEntre (elLoboAlfa l1) (elegirEntre (elLoboAlfa l2) (nombre, 0))
elLoboAlfa (Cazador    nombre presas l1 l2 l3 ) =  elegirEntre (nombre, length presas)
                                                    (elegirEntre (elLoboAlfa l1)
                                                                 (elegirEntre (elLoboAlfa l2)
                                                                              (elLoboAlfa l3)))
                                                  

elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (nom1, c1) (nom2, c2) = if (c1>=c2) then (nom1, c1)
                                                else (nom2, c2)


-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
-- pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M lobo) = losQueExploraronT t lobo

losQueExploraronT :: Territorio -> Lobo -> [Nombre] 
losQueExploraronT t (Cria       nombre                 ) = []
losQueExploraronT t (Explorador nombre ts l1 l2        ) = singularSi nombre (estaEnLaLista' t ts) ++ (losQueExploraronT t l1 ++ losQueExploraronT t l2)
losQueExploraronT t (Cazador    nombre presas l1 l2 l3 ) = losQueExploraronT t l1 ++ losQueExploraronT t l2 ++ losQueExploraronT t l3 

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi _ False = []


estaEnLaLista' :: Territorio -> [Territorio] -> Bool
estaEnLaLista' territorio [] = False 
estaEnLaLista' territorio (t:ts) =  territorio == t || estaEnLaLista' territorio ts

-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
-- dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = combinar (sinRepetir (territoriosL lobo)) lobo 

territoriosL :: Lobo -> [Territorio] 
territoriosL (Cria _                             ) = []
territoriosL (Explorador _      territorios l1 l2) = territorios ++ territoriosL l1 ++  territoriosL l2
territoriosL (Cazador    _            _ l1 l2 l3 ) = territoriosL l1 ++  territoriosL l2 ++ territoriosL l3

combinar :: [Territorio] -> Lobo -> [(Territorio, [Nombre])]
combinar [] l = []
combinar (t:ts) lobo = (t , losQueExploraronT t lobo) : combinar ts lobo 

sinRepetir :: [Territorio] -> [Territorio] 
sinRepetir [] = []
sinRepetir (t : ts) = singularSi t (not (estaEn t ts)) ++ sinRepetir ts 

estaEn :: Territorio -> [Territorio] -> Bool
estaEn t [] = False
estaEn t (t1:ts) = t == t1 || estaEn t ts

-- Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
-- cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador nombre (M lobo) = superioresDelCazadorL nombre lobo 


superioresDelCazadorL  :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL n (Cria       nombre            ) = []
superioresDelCazadorL n (Explorador nombre _ l1 l2    ) = superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2
superioresDelCazadorL n (Cazador    nombre _ l1 l2 l3 ) = singularSi nombre (esSuperior n l1 l2 l3 ) ++ superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2 ++ superioresDelCazadorL n l3
                
esSuperior :: Nombre -> Lobo -> Lobo -> Lobo -> Bool
esSuperior n l1 l2 l3 =  esSuperiorAL n l1  || esSuperiorAL n l2 || esSuperiorAL n l3

esSuperiorAL :: Nombre -> Lobo -> Bool 
esSuperiorAL n (Cria       nombre            ) = False 
esSuperiorAL n (Explorador nombre _ l1 l2    ) = esSuperiorAL n l1 || esSuperiorAL n l2
esSuperiorAL n (Cazador    nombre _ l1 l2 l3 ) =  n == nombre || esSuperiorAL n l1 || esSuperiorAL n l2 || esSuperiorAL n l3 
