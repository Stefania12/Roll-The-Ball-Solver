{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  FlexibleContexts, InstanceSigs #-}

module AStarHeuristic where
import RollTheBall
import ProblemState
import Pipes

import Data.Array as A
import Data.List as L
import Data.Hashable
import Data.Graph.AStar
import qualified Data.HashSet as H

{-
	Semnătura lui aStar este:
		aStar 	:: (Hashable a, Ord a, Ord c, Num c)
			=> (a -> HashSet a)
			-> (a -> a -> c)
			-> (a -> c)
			-> (a -> Bool)
			-> a
			-> Maybe [a], a fiind tipul Level.
	Vom discuta mai jos despre fiecare parametru în parte.
-}

{-
 	*** TODO ***
	
	O constrângere pe care trebuie să o îndeplinească Level pentru a apela
	AStar pe graful nostru este ca Level să fie instanță a lui Hashable.
	Pentru a face asta, trebuie să definim funcția hashWithSalt. Aceasta
	va primi un Int, Level-ul nostru și va returna un Int, adica hash-ul
	rezultat.

	Hint: Dacă un tip de dată 'a' este instanță de Hashable, atunci și
	'[a]' este. Pe considerentul acesta, o sugestie ar fi de defini o
	funcție care extrage din reprezentarea lui Level, Cells sub formă de
	listă sau lista de liste. Următorul pas ar fi să instanțiem Cell pe
	Hashable. Char este, de asemenea, instanța Hashable.

	În cazul acesta,  avem

	    hashWithSalt i level = hashWithSalt i $ toList level
 -}
levelToList :: Level -> [Cell]
levelToList (Level m) = A.elems m

instance Hashable Cell where
    hashWithSalt :: Int -> Cell -> Int
    hashWithSalt n (Cell p m) = hashWithSalt n p

instance Hashable Level where
    hashWithSalt :: Int -> Level -> Int
    hashWithSalt n lvl = hashWithSalt n $ levelToList lvl
{-
	*** TODO ***
	
	Primul parametru al lui aStar ne va returna graf-ul pe care îl
	parcurgem. Acesta este sub forma unei funcții care primește drept
	parametru un nod, aici un Level, și întoarce vecinii săi, sub formă de
	HashSet.

	Hint: fromList
-}

neighbours :: (Level -> H.HashSet Level)
neighbours lvl = H.fromList $ L.map (\(pos_dir, lv) -> lv) $ successors lvl

{-
 	*** TODO ***

	Urmează distanța dintre noduri. Aceasta este o funcție care primește
	două elemente de tip Level și întoarce un număr care reprezintă
	distanța dintre ele.

	Atenție! Aceasta va fi apelată doar pe nivele adiacente!
 -}
 
distance :: (Num c) => (Level -> Level -> c)
distance _ _ = 1 

{-
	Urmează euristica folosită.

	Primul apel pe aStar va fi folosind o euristică banală, care întoarce 1,
	indiferent ne nivel.
-}
trivialHeuristic :: (Num a) => Level -> a
trivialHeuristic _ = 1

{-
	*** TODO ***
	
	Dacă există, aStar returnează un drum optim de către nodul final,
	excluzând nodul de început.
-}
nonTrivialHeuristic :: (Num c) => Level -> c
nonTrivialHeuristic (Level m) = 
    let
        (bound_x, bound_y) = (snd.bounds) m
        num_con_vert = L.length $ L.filter (\(i,j) -> connection_ver (m A.! (i,j)) (m A.! (i+1,j))) [(i,j) | i <- [0..(bound_x-1)], j <- [0..bound_y]]
        num_con_hor = L.length $ L.filter (\(i,j) -> connection_hor (m A.! (i,j)) (m A.! (i,j+1))) [(i,j) | i <- [0..bound_x], j <- [0..(bound_y-1)]]

        num_non_empty = L.length [(i,j) | i<-[0..bound_x], j<-[0..bound_y], not (L.elem (pipe (m A.! (i,j))) [emptySpace, emptyCell])]
    in fromIntegral (num_non_empty - num_con_hor - num_con_vert)

{-
 	*** TODO ***

	Penultimul parametru este o funcție care verifică dacă nodul curent
	este final, sau nu.
 -}
isGoalNode :: Level -> Bool
isGoalNode lvl = isGoal lvl 

{-
 	Ultimul parametru dat lui aStar reprezintă nodul de la care se
	începe căutarea.

	Dacă există, aStar returnează un drum optim de către nodul final,
	excluzând nodul de început.
-}
