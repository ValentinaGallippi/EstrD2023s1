module EscuelaDeMagia
    (EscuelaDeMagia, fundarEscuela, estaVacia, registrar, magos, hechizosDe, leFaltanAprender, egresarUno, enseñar)
where 
data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
{-
INV.REP. 
sea (EDM h mn pqm) 
* si un mago se encuentra en mn tambien se encuentra en pqm y viceversa.
* si hay un hechizo en h entonces este hechizo fue aprendido por algun mago existente en la escuela. 
* no existen dos magos con el mismo nombre en pqm.
* todos los nombres del map tienen que estar asociados a un mago con dicho nombre
-}


import MapV1
import SetV1
import PriorityQueueV1
import Mago


-- Propósito: Devuelve una escuela vacía.
-- Eficiencia: O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptySet emptyM emptyPQ

-- Propósito: Indica si la escuela está vacía.
-- Eficiencia: O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM _ _ pqm) = isEmptyPQ pqm 

-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
-- Eficiencia: O(log M)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar n (EDM h mn pqm) = 
    case lookupM n mn of 
        Nothing -> EDM h (assocM n (crearMago n) mn) (insertPQ n pqm)
        Just x  -> EDM h mn pqm

-- Propósito: Devuelve los nombres de los magos registrados en la escuela.
-- Eficiencia: O(M)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM h mn pqm) = keys mn

-- Propósito: Devuelve los hechizos que conoce un mago dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe n (EDM h mn pqm) = 
    case lookupM n mn of 
        Nothing -> error "el mago dado no pertenece a la escuela dada"
        Just m  -> hechizos m 

-- Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender n (EDM h mn pqm) = 
    case lookupM n mn of 
        Nothing -> error "el mago dado no pertenece a la escuela dada"
        Just m  -> sizeS h - sizeS (hechizos m) 

-- Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- Precondición: Hay al menos un mago.
-- Eficiencia: O(log M)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM h mn pqm) =
    let maxM           = maxPQ pqm
        escuelaSinMago = (EDM h (deleteM maxM mn) (deleteMaxPQ pqm))
    in  (maxM , escuelaSinMago)


-- Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
-- Nota: No importa si el mago ya conoce el hechizo dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(M log M + log H)
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar h' n (EDM h mn pqm) = 
    let newMago = case lookupM n mn of 
                        Nothing -> error "el mago dado no pertenece a la escuela dada"
                        Just m  -> aprender h' m 
    in (EDM (addS h' h) (assocM n newMago mn) (cambiarPor n newMago pqm)) 



cambiarPor :: Nombre -> Mago -> PriorityQueue Mago -> PriorityQueue Mago
-- PRECOND:  el mago de nombre dado existe en la pq. 
cambiarPor n m pq = 
    if nombre (maxPQ pq) == nombre then insertPQ m (deleteMaxPQ pq)
                                   else insertPQ (maxPQ pq) (cambiarPor n m (deleteMaxPQ pq))


