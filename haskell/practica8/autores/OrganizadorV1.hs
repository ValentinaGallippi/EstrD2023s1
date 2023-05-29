module Organizador 
    (Organizador,nuevo,agregarPrograma, todosLosProgramas,autoresDe,programasDe, programaronJuntas, nroProgramasDePersona)
where

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) (Maybe Checksum)


{-
INV.REP : 
    * todas las personas que esten asociadas a un checksum tambien estan en el map de persona, con dicho cheksum asociado a esa persona. 
    * todods los checksum del map Checksum estan asociados a todas las personas de su set en el map de persona. 
    * el maybe siempre tiene el Checksum de todos los que existen en la organizacion con mayor cantidad de autores. 

-}

import SetV1
import MapV1


elMayorPrograma :: Organizador -> Maybe Checksum
-- Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
-- Nothing si no puede devolver un programa.
-- Eficiencia: O(1) en peor caso.
elMayorPrograma (MkO mc mp maxC) = maxC 
 
-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo :: Organizador
nuevo = MkO emptyM emptyM Nothing 

-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO mc mp maxC) c p = 
    let newMC = assocM c p mc
        newMP = asociarPersonas (setToList p) c mp  
    in MkO newMC newMP (maximo maxC c) 

maximo :: Maybe Checksum -> Checksum -> Maybe Checksum
maximo mc c = 
    case mc of 
        Nothing -> Just c 
        Just v  -> Maybe (max c v)

asociarPersonas :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
asociarPersonas []  c mp    = mp 
asociarPersonas (p:ps) c mp = 
    case lookupM p mp of 
        Nothing -> assocM p (addS c (emptySet)) (asociarPersonas ps c mp)
        Just v  -> assocM p (addS c (v))        (asociarPersonas ps c mp)


-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgamas (MkO mc mp maxC) = keys mc 

-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mc mp maxC) c = 
    case lookupM c mc of 
        Nothing -> error "no existe un programa con dicho checksum en el organizador"
        Just v  -> v 

-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
autoresDe (MkO mc mp) p = 
    case lookupM p mp of 
        Nothing -> error "no existe dicha persona en el organizador"
        Just v  -> v 

-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas (MkO mc mp) p1 p2 = 
    let setDe p m = case lookupM p m of 
                        Nothing -> error "no existe la persona dada en el organizador"
                        Just v  -> v
    in compartenSet (setDe p1 mp) (setToList (setDe p2 mp))

compartenSet :: Set Checksum -> [Checksum] -> Bool 
compartenSet s [] = False 
compartenSet s (c:cs) = belongs c s || compartenSet s cs 


-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona (MkO mc mp) p = 
    case lookupM p mp of 
        Nothing -> 0 
        Just v  -> sizeS v 


        