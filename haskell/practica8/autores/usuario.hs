import Organizador
-- (Organizador,nuevo,agregarPrograma, todosLosProgramas,autoresDe,programasDe, programaronJuntas, nroProgramasDePersona)

-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
-- programaron juntas.
-- COSTO: 
--  * programasDe : O(Log P) (p siendo la cantidad total de personas del organziador)
--  * unionS : O(C Log C) (c siendo la cantiad total de programas de los sets.)
-- Total = O(Log P + C Log C )
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 o = 
    unionS (programasDe o p1) (programasDe o p2)

-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
-- COSTO :
-- * lenght : O(C) -> siendo C la cantidad total de programas del organizador. 
-- * todosLosProgamas O(C) -> en peor caso, donde C es la cantidad de códigos en el organizador.
-- * nroProgramasPersona : O(log P) en peor caso, donde P es la cantidad de personas del organizador.
-- Total = O (C + Log P)
esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker o p = length (todosLosProgramas o) == nroProgramasDePersona p o 