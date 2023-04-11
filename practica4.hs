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




