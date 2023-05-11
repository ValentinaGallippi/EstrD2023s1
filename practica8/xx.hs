
import MapV1 
import RAL 

-- O(n)
esBST :: Ord a => Tree a -> Bool
esBST EmptyT           = True
esBST (NodeT x ti td)  = esMayorA x ti && esMenor x td && (esBST ti) && (esBST td)

esMayorA :: Ord a => a -> Tree a -> Bool -- O(1)
esMayorA x EmptyT          = True
esMayorA x (NodeT y ti td) = x > y 

esMenor :: Ord a => a -> Tree a -> Bool -- O(1)
esMenor x EmptyT          = True
esMenor x (NodeT y ti td) = x < y 


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

arbolito :: Tree Char
arbolito = NodeT 'f' (NodeT 'd' (NodeT 'b' EmptyT EmptyT) (NodeT 'e' EmptyT EmptyT)) (NodeT 'm'(NodeT 'j' (NodeT 'i' EmptyT EmptyT)(NodeT 'l' EmptyT EmptyT)) (NodeT 'r' (NodeT 'n' EmptyT EmptyT)(NodeT 't' EmptyT EmptyT)))


balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT x t1 t2) =  abs (heightT t1 - heightT t2) <= 1 && balanceado t1 && balanceado t2 


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA x EmptyT = Nothing
elMaximoMenorA x (NodeT y ti td) = 
    if (x<=y) then elMaximoMenorA x ti
              else  case elMaximoMenorA x td of 
                           Nothing -> Just y
                           Just v -> Just v 


elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA x EmptyT = Nothing
elMinimoMayorA x (NodeT y ti td) = 
    if (x>=y) then elMinimoMayorA x td
              else  case elMinimoMayorA x ti of 
                           Nothing -> Just y
                           Just v -> Just v 


ral :: RAList Char 
ral = 
      add 'n'
    $ add 'e'
    $ add 'l'
    $ add 'v'
    $ emptyRAL

map1 :: Map Int Char 
map1 = 
      assocM 3 'n'
    $ assocM 2 'e'
    $ assocM 1 'l'
    $ assocM 0 'v'
    $ emptyM 


fromJust :: Maybe v -> v
fromJust (Just v) = v 

