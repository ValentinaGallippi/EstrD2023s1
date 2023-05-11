module Empresa 
        (ConsE, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, borrarEmpleado)
    where

type SectorId = Int
type CUIL = Int


data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)
{-Inv. Rep.: en ConsE ms mc 
            * si un empleado esta en ms tambien esta en mc, por ende los maps siempre tienen que tener la misma cantidad de empleados. 
            * los empleados en el Set TIENEN que trabajar en el set dado (dentrode los secotres de ese empleado va a estar ese sectorId)-}

import Empleado
-- (Empleado,consEmpleado,cuil,incorporarSector,sectores)

import MapV1
-- (Map, emptyM, assocM, lookupM, deleteM, keys) 

import SetV1
-- (Set, emptySet, addS, belongs, sizeS, removeS, unionS, setToList) 

-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

-- Propósito: devuelve el empleado con dicho CUIL.
-- Costo: O(log E)
buscarPorCUIL :: CUIL -> Empresa -> Empleado 
-- PRECONDICION : el empleado tiene que estar en mc 
buscarPorCUIL c (ConsE _ mc) = 
       case lookupM c mc of 
       Nothing -> error "no existe el empleado dado en la empresa"
       Just v -> v 


-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(logS + E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector sector (ConsE ms _) =  
        case lookupM sector ms of 
            Nothing -> []
            Just v  -> setToList v 


-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ mc) = keys mc 

-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE ms _) = keys ms

-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(logS)
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector sectorId (ConsE ms mc) = ConsE (assocM sectorId emptySet ms) mc

-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
-- el CUIL dado.
-- Costo: calcular.

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sectores c (ConsE ms mc) = ConsE  (asociarASectores sectores c ms)  (assocM c (empleado sectores c) mc)

-- asociarASectores + assocM + empleado 

asociarASectores :: [SectorId] -> CUIL -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
asociarASectores [] c map = map 
asociarASectores (s:sc) c map = 
    case lookupM s map of 
        Nothing -> assocM s (empleado (s:sc) c) (asociarASectores sc c map)
        Just v  -> assocM s (addS (empleado (s:sc) c) v) (asociarASectores sc c map)
-- lookUpM + assocM + addS + empleado 
-- Log S   + Log S  + 

empleado :: [SectorId] -> CUIL -> Empleado 
empleado [] c     = consEmpleado c
empleado (s:sc) c = incorporarSector s (empleado sc c)

-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: calcular.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa 
-- PRECON: tiene que existir el empleado con dicho CUIL en la empresa. 
agregarASector id c (ConsE ms mc) = let empleado = fromJust (lookUpM c mc)
    ConsE  (agregarEnSectores (sectores empleado) (incorporarSector id empleado) ms)  (assocM c (incorporarSector id empleado) mc)

-- COSTO 
{- fromJust + lookUpM + agregarEnSectores + sectores + incorporarSector + assocM 
   1        + Log E   + S* (Log S + Log E)+ 1        + Log S            + Log E 
   (Log S + Log E) + S * (Log S + Log E)
   (Log S + Log E) * (S+1)
   (Log S + Log E) * S
    S * (Log S + Log E)
-}

agregarEnSectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEnSectores [] e ms       = ms 
agregarEnSectores (id:ids) e ms = 
    case lookUpM id ms of 
        Nothing        -> assocM id (addS e (emptySet)) (agregarEnSectores ids e ms)
        Just empleados -> assocM id (addS e empleados)  (agregarEnSectores ids e ms)
{-COSTO : 
S * (lookUpM + assocM + addS) 
S *  (Log S   + Log S  + Log E)
S *  (Log S + Log E) 

-}


-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: calcular.
--PRECONDICION: el empleado esta en la empresa. 
borrarEmpleado :: CUIL -> Empresa -> Empresa 
borrarEmpleado c (ConsE ms mc) = 
    case lookupM c mc of
        Nothing -> ConsE ms mc
        Just e  -> ConsE (borrarEnSectores e (sectores e) ms) (deleteM c mc)

borrarEnSectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado) 
borrarEnSectores e [] map = map
borrarEnSectores e (id:ids) map =
    case lookupM id map of 
        Nothing        -> borrarEnSectores e ids map
        Just empleados -> assocM id (removeS e empleados) (borrarEnSectores e ids map)

{-Costo: S*(Log S + Log E)  
    * borrarEmpleado : 
    lookUp + borrarEnSectores   + sectores + deleteM
    Log E + S * (Log S + Log E) + 1        + Log E
    2Log E + S * (Log S + Log E) 
    Log E  + S * Log S + S * Log E
    S * Log S + S * Log E + Log E
    S * Log S + Log E * (S +1)
    S*  Log S + S * Log E
    S*(Log S + Log E)  
    * borrarEnSectores : S * (Log S + Log E)
    (lookUpM + assocM + removeS) * S 
    (Log S    + Log S  + Log E ) * S
    (2 Log S + Log E ) * S
    S * (Log S + Log E)
 -}