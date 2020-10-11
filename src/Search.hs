{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.List as L
import Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {state::s, action::Maybe a, parent::Maybe (Node s a), depth::Int, children::[Node s a]}

instance Eq s => Eq (Node s a)
    where n1 == n2 = state n1 == state n2

instance Show s => Show (Node s a)
    where show n = show (state n)


{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState node = state node

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = parent node

nodeDepth :: Node s a -> Int
nodeDepth node = depth node

nodeAction :: Node s a -> Maybe a
nodeAction node = action node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = children node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace lvl = 
    let
        make_node = \st act p d -> Node st act p d (L.filter (\node -> state node /= st) (L.map (\(ac, lv) -> make_node lv (Just ac) (Just (make_node st act p d)) (d+1)) (successors st)))
    in make_node lvl Nothing Nothing 0

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]

bfs x =
    let
        bfs_helper = \(_, fronteer) my_set -> if (L.null fronteer)
                                                        then []
                                                        else 
                                                            let
                                                                chl = L.filter (\node->not (S.member (state node) my_set)) ((children . L.head) fronteer)
                                                                l_add = (chl, (tail fronteer)++chl)
                                                            in l_add : (bfs_helper l_add (S.union my_set (S.fromList (L.map (state) chl))))
    in ([x],[x]) : (bfs_helper ([x],[x]) (S.fromList [state x]))

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS node1 node2 = 
    let
        zipped_front1 = zipWith (\x y -> (x, y)) (bfs node1) (bfs node2)
        zipped_front2 = zipWith (\x y -> (x, y)) (tail (bfs node1)) (bfs node2)

        f = \((a1,f1),(a2,f2)) -> 
                                let
                                    i1 = [x | x <- a1, elem x f2]
                                    i2 = [x | x <-a2, elem x f1]
                                in if (L.null i1)
                                    then if (L.null i2)
                                            then []
                                            else [L.head [x | x <- f1, state x == state (L.head i2)], L.head i2]
                                    else [L.head i1, L.head [x | x <- f2, state x == state (L.head i1)]]

        intersection1 = L.map (f) zipped_front1
        intersection2 = L.map (f) zipped_front2
        intersections = zipWith (\x y-> (x,y)) intersection1 intersection2

        intersections_mapped = L.map (\(i1,i2)-> if (L.null i1) then i2 else i1) intersections

        intersection_nodes = L.head $ L.filter (\l-> not (L.null l)) intersections_mapped

    in (L.head intersection_nodes, (L.head . L.tail) intersection_nodes) 

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath node = 
    let
        f Nothing = Nothing
        f (Just n) = Just (n, parent n)
        rev_path = L.unfoldr (f) (Just node)

        rev_path_mapped = L.map (\n -> (action n, state n)) rev_path
    in reverse rev_path_mapped


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve start stop = 
            let
                (n1,n2) = bidirBFS (createStateSpace start) (createStateSpace stop)
                p1 = extractPath n1
                p2 = L.map (\(Just act, st) -> let rev_act = reverseAction (act,st) in (Just (fst rev_act), snd rev_act)) $ reverse $ tail $ extractPath n2
            in p1++p2
