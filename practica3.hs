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



