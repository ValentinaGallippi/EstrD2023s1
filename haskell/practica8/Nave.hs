module Nave 
    (interfaz)
        where

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
{- 
INV. REP : 
    sea (N ms mn mht)
    * todos los tripulantes que esten en ms estan en mht y viceversa. 
    * No hay Tripulantes repetidos en mht
    * todos los sectores a los que esten asignados los tripulantes existen en ms 
    * si un sector tiene asignado un tripulante, entonces ese tripulante tiene asignado dicho sector. 
-}


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

import MapV1
import MaxHeap 

-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S)
construir :: [SectorId] -> Nave
construir s = (N (asociarSectores s emptyM) emptyM emptyH)

asociarSectores :: [SectorId] -> Map SectorId Sector -> Map SectorId Sector
asociarSectores [] m       = m
asociarSectores (id:ids) m = assocM id (crearSector id) (asociarSectores ids m)

-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mn mht) = let nuevoT = crearT n r 
                              mn'  = assocM n nuevoT mn
                              mht' = insertH nuevoT
                              in (N ms mn' mht') 



-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N ms mn mht) = 
    case lookUpM n mn of 
        Nothing -> error "no existe un tripulante con el nombre dado"
        Just t  ->  sectoresT t 


-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector id (N ms mn mhn) = 
    case lookUpM id ms of 
        Nothing -> error "no existe un sector con dicho id"
        Just s  -> (tripulantesS s, componentesS s)
    
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T)
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N _ _ mht) = tripulantesMH mht

tripulantesMH :: MaxHeap Tripulante -> [Tripulante]
tripulanteMH mht = 
    if isEmptyH mht then [] 
                else maxH mht : tripulantesMH (deleteMax mht)

-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector componentes id (N ms mn mht) = 
    case lookUpM id ms of 
        Nothing -> error "el sector dado no existe" 
        Just s  -> let sectorActualizado = agregarComponentes componetes s 
                   let newMs             = assocM id sectorActualizado ms
                   in N (newMs mn mht)
 
agregarComponentes :: [Componente] -> Sector -> Sector 
agregarComponentes [] s     = s
agregarComponentes (c:cs) s = agregarComponete c (agregarComponentes cs s) 

-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n id (N ms mn mht) = 
    let newS = agregarT n (buscarSector id ms)
        newT = asignarS id (buscarTripulante n mn)
    in N (assocM id newS ms) (assocM n newT mn) (agregarAH newT mht)


buscarSector :: SectorId -> Map SectorId Sector -> Sector 
-- PRECOND: ecxiste el sector dado. 
buscarSector id ms = 
    case lookUpM id ms of 
        Nothing -> error "no existe el sector dado"
        Just s  -> s 


buscarTripulante :: Nombre -> Map Nombre Tripulante -> Tripulante 
-- PRECOND: ecxiste el tripulante dado. 
buscarTripulante n mn = 
    case lookUpM n mn of 
        Nothing -> error "no existe el sector dado"
        Just t  -> t

-- costo O(t log t)
agregarAH :: Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
agregarAH t mht =
    if nombre (maxH mht) == nombre t then insertH t          (deleteMax mht)
                                     else insertH (maxH mht) (agregarAH t (deleteMax mht))


---------------------------------------------------------------------------------------------------
-- ========== usuario ==========
-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores :: Nave -> Set SectorId
sectores nave = 
    let tripulantes = tripulantesN nave
    in sectoresDe tripulantes nave

sectoresDe :: [Tripulante] -> Nave -> Set SectorId --- PREGUNTAR SI PUEDO USAR COSAS DE SET : las uso porque como se que me devuelve un setr, onda no estaria viendo la implementacion. 
sectoresDe []  n    = emptyS
sectoresDe (t:ts) n = unionS (sectoresAsignados (nombre t) nave) (sectoresDe ts n)

-- costo de sectoresDe :
-- emptyS + T + unionS  + sectoresAsignados
--          T * (S Log S + Log T )
-- costo total = 

-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados n = tripulantesSinSectores (tripulantes n) n
-- O(Log T + tripulantesSinSectores)
-- (Log T + T*Log T)
-- Log T * (T+1)
-- Costo Total = T * Log T 
tripulantesSinSectores :: [Tripulante] -> Nave -> [Tripulante] 
tripulantesSinSectores [] n     = []
tripulantesSinSectores (t:ts) n = 
    if sizeS (sectoresAsignados t n) == 0 then t : tripulantesSinSectores ts n 
                                          else tripulantesSinSectores ts n

-- costo 
-- T * (sizeS + sectoresAsignados) 
-- T * (1 + Log T )
-- Costo Total = T*Log T


-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles :: Nave -> [Barril]
-- asumo que no hay sectores que no tengan tripulantes
barriles nave = let sectores = setToList (sectores nave)
                in barrilesS sectores nave

barrilesS :: [Sector] -> [Barril]
barrilesS [] = ...
barrilesS (s:ss) = 
    (sectorId s) -- preguntar si se puede usar las funciones de sector especificas y si puedo crear otra funcion
    --              que use patter matchin para ver si dentro de los componenentes edl sector hay un barril. 