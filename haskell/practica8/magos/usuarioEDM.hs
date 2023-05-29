impor EscuelaDeMagia 

-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos edm = hechizosDe (magos edm) edm 

hechizosDe :: [Mago] -> EscuelaDeMagia -> Set Hechizo 
hechizos [] edm     = emptySet
hechizos (m:ms) edm = unionS (hehechizosDe m) (hechizos ms edm)

-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- Eficiencia: O(log M)
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto edm =
    let (m,edm') = egresarUno edm
         maxM    = nombre m 
    in leFaltanAprender m edm == 0 

-- Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
-- magos.
-- Eficiencia: O(M log M)
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos edm = 
    if not (hayUnExperto edm) then ([] , edm)
                              else let (m,esc) = egresarUno edm
                                       (ms,edm') = egresarExpertos edm 
                                    in ((m:ms),edm')