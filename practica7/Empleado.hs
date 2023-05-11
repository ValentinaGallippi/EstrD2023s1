module Empleado 
            (Empleado,consEmpleado,cuil,incorporarSector,sectores)
        where 
            
-- Propósito: construye un empleado con dicho CUIL.
-- Costo: O(1)
consEmpleado :: CUIL -> Empleado
consEmpleado = undefined

-- Propósito: indica el CUIL de un empleado.
-- Costo: O(1)
cuil :: Empleado -> CUIL
cuil = undefined 

-- Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
-- Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector :: SectorId -> Empleado -> Empleado
incorporarSector = undefined

-- Propósito: indica los sectores en los que el empleado trabaja.
-- Costo: O(1)
sectores :: Empleado -> SectorId
sectores = undefined 