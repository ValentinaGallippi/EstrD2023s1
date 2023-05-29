module Enterprise
    (Nave, naveVacia, tripulantesDe, sectores, conMayorRango, conMasTripulantes, conRango, sectorDe, agregarTripulante)
where 

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

import SetV1
import MapV1
import Heap

{-
INV. REP : 
sea (MkN ms ht (s,i))
* todos los tripulantes que esten asociados a un sector en ms tienen que existir en ht. 
* en (s,i) s siempre tiene que ser el sector con mas tripulantes, y tiene que existir en ms. 
* los tripulantes que esten en el set asociados a el sector no pueden aparecer en otros sets asociados a otros sectores. Es decir cada tripulante puede estar asociado a un sector como maximo. 
-}

{-
Observaciones: 
* Cada tripulante puede estar en un sector como máximo.
* Se guarda al sector con más tripulantes de la nave y cuántos tripulantes tiene ese sector.
* Los tripulantes se ordenan por rango de mayor a menor en la Heap
  (no se confunda, findMin devuelve al tripulante con mayor rango).
-}

naveVacia :: [Sector] -> Nave
-- Propósito: Crea una nave con todos esos sectores sin tripulantes.
-- Precondición: la lista de sectores no está vacía
-- Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia sectores = MkN (asociarSectores sectores emptyM) emptyH ((head sectores), 0)

asociarSectores :: [Sector] -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante) 
asociarSectores []     _ = error "la lista de sectores dada no tiene que ser vacia."
asociarSectores [s]    m = assocM s emptySet m 
asociarSectores (s:ss) m = assocM s emptySet (asociarSectores ss m)  

-- Propósito: Obtiene los tripulantes de un sector.
-- PRECOND : el sector dado tiene que existir 
-- pongo esa precondicion porque si yo lo busco y no esta, no puedo devolver emptySet, porque estoy indicando qeu el sector SI EXISTE, pero no tiene tripulantes.
-- ahora si el sector existe va a devolver emptySet si no tiene tripulantes, o un set con los tripulantes que tenga. 
-- Costo: O(log S) siendo S la cantidad de sectores.
tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe s (MkN ms h maxS) = 
    case lookupM s ms of 
        Nothing  -> error "el sector dado no existe"
        Just set -> set

-- Propósito: Denota los sectores de la nave
-- Costo: O(S) siendo S la cantidad de sectores.
sectores :: Nave -> [Sector]
sectores (MkN ms ht maxS) = keys ms

-- Propósito: Denota el tripulante con mayor rango.
-- Precondición: la nave no está vacía.
-- Costo: O(1).
conMayorRango :: Nave -> Tripulante
conMayorRango (MkN ms ht maxS) = 
    if isEmptyH ht then error "la nave no tiene tripulantes"
                   else findMin ht 

-- Propósito: Denota el sector de la nave con más tripulantes.
-- Costo: O(1).
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MkN ms ht (s,i)) = s 

-- Propósito: Denota el conjunto de tripulantes con dicho rango.
-- Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango :: Rango -> Nave -> Set Tripulante
conRango r (MkN ms ht maxS) = conRangoH r ht 

conRangoH :: Rango -> Heap Tripulante -> Set Tripulante
conRangoH r h = 
    if isEmptyH h then emptySet
                  else if rango (findMin h) = r then addS (findMin h) (conRangoH r (deleteM h))
                                                else conRangoH r (deleteM h) 

-- Propósito: Devuelve el sector en el que se encuentra un tripulante.
-- Precondición: el tripulante pertenece a la nave.
-- Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe :: Tripulante -> Nave -> Sector
sectorDe t (MkN ms ht maxS) = sectorDeMs t ms (keys ms)

-- CONFIAR EN LA RECURSION Y NO PERSAR COMO FUNCIONA......
sectorDeMs :: Tripulante -> Map Sector (Set Tripulante) -> [Sector] -> Sector
sectorDeMs t ms []     = error "no existe el tripulante dado en la nave"
sectorDeMs t ms (s:ss) = 
    case lookupM s ms of 
        Nothing -> error "no existe el sector en la nave"
        Just set -> if belongs t set then s 
                                     else sectorDeMs t ms ss 

-- Propósito: Agrega un tripulante a ese sector de la nave.
-- Precondición: El sector está en la nave. 
-- Costo: No hay datos (justifique su elección).
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
agregarTripulante t s (MkN ms ht maxS) = 
    let ts = case lookupM s ms of 
                  Nothing -> error " no existe el sector dado" 
                  Just set -> set 
    MkN (assocM s (addS t ts) ms)  (insertH t ht)  (verNuevoM s ((sizeS ts) + 1) maxS)

verNuevoM :: Sector -> Int -> (Sector, Int) -> (Sector,Int)
verNuevoM s1 i1 (s2,i2) = 
    if i1 > i2 then (s1,i1) 
               else (s2,i2)