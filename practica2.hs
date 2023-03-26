--1. Recursión sobre listas
--1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--2 
longitud :: [a] -> Int
longitud []     = 0 
longitud (x:xs) = 1 + longitud xs  

--3
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) =  n+1  : sucesores ns

--4 
conjuncion :: [Bool] -> Bool
conjuncion []     =  True 
conjuncion (b:bs) =   b  && conjuncion bs

--5 
disyuncion :: [Bool] -> Bool
disyuncion []     =  False 
disyuncion (b:bs) =   b  || disyuncion bs

--6
 aplanar :: [[a]] -> [a]
