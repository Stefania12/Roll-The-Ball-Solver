{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {pipe::Char, mutable::Bool} deriving Eq

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {matrix::(A.Array Position Cell)}

instance Ord Level
    where (Level m1) <= (Level m2) = 
                                    let
                                        comps = map (\(x,y) -> pipe x <= pipe y) (zipWith (\x y -> (x,y)) (A.elems m1) (A.elems m2))
                                    in and comps

instance Eq Level
    where (Level m1) == (Level m2) = 
                                    let
                                        comps = map (\(x,y) -> pipe x == pipe y) (zipWith (\x y -> (x,y)) (A.elems m1) (A.elems m2))
                                    in and comps

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level 
    where show (Level m) = 
            let

                f1 = \cel -> pipe cel
                chars = map (f1) (elems m)
                cols = map (snd) $ indices m
                zipped = zipWith (\x y -> (x,y)) cols chars
                f :: (Int, Char) -> [(Int, Char)] -> [(Int, Char)]
                f = \p acc -> case acc of
                                    [] -> p : [(0, endl)]
                                    _ -> if (fst p < (fst.head) acc) then p:acc else p:(0, endl):acc
            in showString ('\n': (map (snd) (foldr (f) [] zipped))) ""

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = 
    let
        cell = Cell emptySpace True
        matr = array ((0,0),pos) [((i,j), cell) | i <- [0..(fst pos)], j <- [0..(snd pos)]]
    in Level matr

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (chr, pos) (Level m) =
    let
        cell
            | elem chr startCells = Cell chr False
            | elem chr winningCells = Cell chr False
            | otherwise = Cell chr True
        lvl_cell = m A.! pos
        pos_ok = elem (fst pos) [0..(fst ((snd.bounds) m))] && elem (snd pos) [0..(snd ((snd.bounds) m))]
    in if (pos_ok == True)
            then if (pipe lvl_cell == emptySpace) then Level (m A.// [(pos, cell)]) else Level m
            else Level m


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel corner l = foldr (addCell) (emptyLevel corner) l


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos North (Level m) =
    let
        dir_ok = fst pos > 0
        cell = m A.! pos
        neighbor_cell = m A.! ((fst pos)-1, snd pos)
    in if (dir_ok && pipe neighbor_cell == emptySpace && mutable cell)
            then Level $ m A.// [(pos, neighbor_cell), (((fst pos)-1, snd pos), cell)]
            else Level m

moveCell pos South (Level m) =
    let
        corner = (snd.bounds) m
        dir_ok = fst pos < fst corner
        cell = m A.! pos
        neighbor_cell = m A.! ((fst pos)+1, snd pos)
    in if (dir_ok && pipe neighbor_cell == emptySpace && mutable cell)
            then Level $ m A.// [(pos, neighbor_cell), (((fst pos)+1, snd pos), cell)]
            else Level m

moveCell pos West (Level m) =
    let
        dir_ok = snd pos > 0
        cell = m A.! pos
        neighbor_cell = m A.! (fst pos, (snd pos)-1)
    in if (dir_ok && pipe neighbor_cell == emptySpace && mutable cell)
            then Level $ m A.// [(pos, neighbor_cell), ((fst pos, (snd pos)-1), cell)]
            else Level m

moveCell pos East (Level m) =
    let
        corner = (snd.bounds) m
        dir_ok = snd pos < snd corner
        cell = m A.! pos
        neighbor_cell = m A.! (fst pos, (snd pos)+1)
    in if (dir_ok && pipe neighbor_cell == emptySpace && mutable cell)
            then Level $ m A.// [(pos, neighbor_cell), ((fst pos, (snd pos)+1), cell)]
            else Level m

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-} 
connection_hor :: Cell -> Cell -> Bool
connection_hor cell1 cell2
                | elem (pipe cell1) [horPipe, topLeft, botLeft, startRight, winRight] = elem (pipe cell2) [horPipe, botRight, topRight, startLeft, winLeft]
                | otherwise = False

connection_ver :: Cell -> Cell -> Bool
connection_ver cell1 cell2
                | elem (pipe cell1) [verPipe, topLeft, topRight, startDown, winDown] = elem (pipe cell2) [verPipe, botLeft, botRight, startUp, winUp]
                | otherwise = False


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

wonLevel :: Level -> Bool
wonLevel (Level m) =
    let
        start_pos = (fst.head) [x | x <- assocs m, elem ((pipe.snd)x) startCells]
        connection_left = \pos m1 -> snd pos > 0 && connection_hor (m1 A.! (fst pos, snd pos -1)) (m1 A.! pos)
        connection_right = \pos m1 -> snd pos < (snd.snd) (bounds m1) && connection_hor (m1 A.! pos) (m1 A.! (fst pos, snd pos +1))
        connection_up = \pos m1 -> fst pos > 0 && connection_ver (m1 A.! (fst pos -1, snd pos)) (m1 A.! pos)
        connection_down = \pos m1 -> fst pos < (fst.snd) (bounds m1) && connection_ver (m1 A.! pos) (m1 A.! (fst pos +1, snd pos))

        can_find_win = \pos m1 -> if (elem (pipe (m A.! pos)) winningCells)
                                    then True
                                    else
                                            let
                                                new_m = m1 A.// [(pos, Cell emptySpace True)]
                                                can_find_win_left = connection_left pos m1 && can_find_win (fst pos, (snd pos) -1) new_m
                                                can_find_win_right = connection_right pos m1 && can_find_win (fst pos, (snd pos) +1) new_m
                                                can_find_win_up = connection_up pos m1 && can_find_win((fst pos) -1, snd pos) new_m
                                                can_find_win_down = connection_down pos m1 && can_find_win ((fst pos) +1, snd pos) new_m
                                            in can_find_win_left || can_find_win_right || can_find_win_up || can_find_win_down
    in can_find_win start_pos m

instance ProblemState Level (Position, Directions) where
    successors (Level m) = 
        let
            moves = [((i,j), dir) | i<-[0..(fst.snd)(bounds m)], j<-[0..(snd.snd)(bounds m)], dir <-[North, South, West, East]]

            is_move_valid  = \mv -> not (moveCell (fst mv) (snd mv) (Level m) == (Level m))
            valid_moves = filter (is_move_valid) moves
            get_action_state = \pos_dir -> (pos_dir, moveCell (fst pos_dir) (snd pos_dir) (Level m))
        in map (get_action_state) valid_moves




    isGoal = wonLevel
    reverseAction ((pos, North), lvl) = 
                                    let
                                        rev_pos = (fst pos -1, snd pos)
                                        rev_dir = South
                                    in ((rev_pos, rev_dir), moveCell rev_pos rev_dir lvl)

    reverseAction ((pos, South), lvl) = 
                                    let
                                        rev_pos = (fst pos +1, snd pos)
                                        rev_dir = North
                                    in ((rev_pos, rev_dir), moveCell rev_pos rev_dir lvl)

    reverseAction ((pos, West), lvl) = 
                                    let
                                        rev_pos = (fst pos, snd pos -1)
                                        rev_dir = East
                                    in ((rev_pos, rev_dir), moveCell rev_pos rev_dir lvl)

    reverseAction ((pos, East), lvl) =
                                    let
                                        rev_pos = (fst pos, snd pos +1)
                                        rev_dir = West
                                    in ((rev_pos, rev_dir), moveCell rev_pos rev_dir lvl)
