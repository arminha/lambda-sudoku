--
-- Copyright (C) 2012  Armin Häberling
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Sudoku
    (Numbers(One,Two,Three,Four,Five,Six,Seven,Eight,Nine)
    ,NumberSet
    ,Field
    ,Index
    ,Table
    ,Action(Enter)
    ,constructTable
    ,enter
    ,solve
    ,nextStep
    ,randSolve
    ,prettyTable
    ,prettyField
    ,toString
    ,newPuzzle
    )

where

-- todo
{- consistency check prior to solving - to avoid long computation on invalid starting states -}

-- imports

import Utils
import Sudoku.Table

import Data.Array
import Data.List
import System.Random
import Data.Maybe


toString :: Numbers -> String
toString = show . fromEnum
--toString n = ["A","E","I","O","P","R","S","T","X"]!!((fromEnum n) -1)


-- sudoku solver

type Solver = [Strategy] -> Table -> Table


data Action = Enter Numbers Index | Remove Numbers Index
	    deriving (Show,Eq)
type Strategy = Table -> [Action]

doAction :: Action -> Table -> Table
doAction (Enter n i) = enter n i
doAction (Remove n i) = remove n i

doActions :: [Action] -> Table -> Table
doActions al t = foldl (\x y -> y x) t $ map doAction al

rowN :: Int -> [Index]
rowN n
    | (n >= 0) && (n < 9) = [(n,i) | i <- [0..8]]
    | otherwise = error "Sudoku.rowN: Out of bound"

columnN :: Int -> [Index]
columnN n
    | (n >= 0) && (n < 9) = [(i,n) | i <- [0..8]]
    | otherwise = error "Sudoku.columnN: Out of bound"

quadNN :: Index -> [Index]
quadNN (n,m)
    | (n >= 0) && (n < 3) && (m >= 0) && (m < 3) = let ind x = map (+(x*3)) [0..2]
						   in [(i,j) | i <- (ind n), j <- (ind m)]
    | otherwise = error "Sudoku.quadNN: Out of bound"


-- strategy 1: enter Numbers which are alone

simpleStrategy :: Strategy
simpleStrategy t = case uniquelist t of
		   (i, Right [n]):_ -> [Enter n i]
		   [] -> []

uniquelist :: Table -> [(Index,Field)]
uniquelist t = [(i,f) | (i,f) <- assocs t, isAlone f]
    where isAlone :: Field -> Bool
	  isAlone (Right [_]) = True
	  isAlone _ = False

-- strategy 2: check columns, rows and quads for unique appearances

appearance :: Table -> [Index] -> Numbers -> [Index]
appearance t indl num = [ind | ind <- indl, hasNum $ t!ind]
    where hasNum (Left _) = False
	  hasNum (Right nl) = elem num nl

uniqueAppList :: Table -> [(Index,Numbers)]
uniqueAppList t = rest ((map (uncurry (quadAppearance t)) quadArgs) 
			++ (map (uncurry (columnAppearance t)) columnRowArgs)
			++ (map (uncurry (rowAppearance t)) columnRowArgs))
    where columnRowArgs = [(r,n) | r <- [0..8], n <- [One .. Nine]]
	  quadArgs = [((i,j),n) | i <- [0..2], j <- [0..2], n <- [One .. Nine]]
	  rest = concat . (filter (\x -> length x == 1))
	  columnAppearance :: Table -> Int -> Numbers -> [(Index,Numbers)]
	  columnAppearance t column num = zip (appearance t (columnN column) num) (repeat num)
	  rowAppearance :: Table -> Int -> Numbers -> [(Index,Numbers)]
	  rowAppearance t row num = zip (appearance t (rowN row) num) (repeat num)
	  quadAppearance :: Table -> Index -> Numbers -> [(Index,Numbers)]
	  quadAppearance t quad num = zip (appearance t (quadNN quad) num) (repeat num)

secondStrategy :: Strategy
secondStrategy t = case uniqueAppList t of
		   (i,n):_ -> [Enter n i]
		   [] -> []

-- strategy 3: quad vs columns and rows

f :: Table -> Numbers -> ([Index],[Index],[Index]) -> [Action]
f t num (inter,quad,line) = case map (\x -> appearance t x num) [quad,inter,line] of
			    [[],(_:_),l@(_:_)] -> map (Remove num) l
			    _ -> []

columnIndices :: [([Index],[Index],[Index])]
columnIndices = map (\x -> g (h x) (i x)) [(r,c) | r <- [0..2], c <- [0..8]]
    where g col quad = (intersect col quad,quad \\ col,col \\ quad)
	  h (_,c) = columnN c
	  i (r,c) = quadNN (r,c `div` 3)

rowIndices :: [([Index],[Index],[Index])]
rowIndices = map (\x -> g (h x) (i x)) [(r,c) | r <- [0..8], c <- [0..2]]
    where g row quad = (intersect row quad,quad \\ row,row \\ quad)
	  h (r,_) = rowN r
	  i (r,c) = quadNN (r `div` 3,c)

thirdStrategy :: Strategy
thirdStrategy t = firstElem (map (uncurry (f t)) 
			     [(n,indLists) | n <- [One .. Nine], 
			      indLists <- (columnIndices ++ rowIndices)])

-- strategy 4: backtracking - very ugly implementation until now

sortList :: Table -> [(Index,Field)]
sortList t = sortBy smaller (filter isRight (assocs t))
	     where isRight (_,(Right _)) = True
		   isRight _ = False
		   smaller (_,Right nl1) (_,Right nl2) = compare (length nl1) (length nl2)

firstSolution :: [(Table,Bool)] -> Table
firstSolution ((t,True):_) = t
firstSolution ((_,False):sl) = firstSolution sl

isSolution :: Table -> Bool
isSolution t = isSolutionHelp (elems t)
    where isSolutionHelp [] = True
	  isSolutionHelp ((Left _):nl) = isSolutionHelp nl
	  isSolutionHelp _ = False

-- end backtracking

solve :: Bool -> Table -> Table
solve bt = solver bt [simpleStrategy,secondStrategy,thirdStrategy]

solver :: Bool -> Solver
solver backtracking strList t = 
    if isUnsolvable t then t
    else 
    case firstElem (map (\x -> x t) strList) of 
    al@(_:_) -> solver backtracking strList (doActions al t)
    [] -> if backtracking
          then case sortList t of -- fallback to backtracking
               (i,Right nl):_ -> let sl = map (\num -> solver backtracking strList (enter num i t)) nl
                                     soll = zip sl (map isSolution sl)
                                     unique = any (\(_,y) -> y) soll -- any or one
				 in if unique
				    then firstSolution soll
				    else t
	       [] -> t
	  else t

-- return next action
nextStep :: Table -> Maybe Action
nextStep t = let steps tab = concat $ map (\x -> x tab) [simpleStrategy,secondStrategy]
	     in if (null $ steps t) 
		then listToMaybe $ steps (doActions (thirdStrategy t) t) 
		else listToMaybe $ steps t


-- solve a Table with backtracking, but use a random order of guesses
randSolve :: Int -> Table -> Table
randSolve n = solverRnd (mkStdGen n) [simpleStrategy,secondStrategy,thirdStrategy]

solverRnd :: RandomGen g => g -> Solver
solverRnd gen strList t = 
    case firstElem (map (\x -> x t) strList) of 
    al@(_:_) -> solverRnd gen strList (doActions al t)
    [] -> case sortList t of -- fallback to backtracking
	  (i,Right nl):_ -> let rndNl = randOrder gen nl
				sl = map (\num -> solverRnd (snd rndNl) strList (enter num i t)) (fst rndNl)
				soll = zip sl (map isSolution sl)
				unique = any (\(_,y) -> y) soll -- any or one
			    in if unique
			       then firstSolution soll
			       else t
	  [] -> t

-- TODO check consistency
isUnsolvable :: Table -> Bool
isUnsolvable t = or (map noNum (elems t))
    where noNum (Right []) = True
          noNum _ = False

-- pretty printed output

prettyTable :: Table -> String
prettyTable t = prettylines (map prettyField (elems t))
    where prettylines l = unlines(map unwords (groupAt 9 l))

prettyField :: Field -> String
prettyField (Left x) = toString x
prettyField (Right xl) = "{" ++ ((concat . (intersperse ",")) (map (toString) xl)) ++ "}"

-- | Create a new puzzle
newPuzzle :: Int -- ^ RG initialization value for distributing numbers on table
          -> Int -- ^ RG initialization value for removing fields
          -> [(Index,Numbers)]
newPuzzle tr rr =
    let initTable = map (\(ind,Left n) -> (ind,n)) $ assocs (randSolve tr emptyTable)
	rRandG = mkStdGen rr
    in removeFields [] $ fst (randOrder rRandG initTable)
    
removeFields :: [(Index,Numbers)] -> [(Index,Numbers)] -> [(Index,Numbers)]
removeFields keep [] = keep
removeFields keep remove = let newRemove = tail remove
			   in if hasSolution (keep ++ newRemove)
			      then removeFields keep newRemove
			      else removeFields ((head remove):keep) newRemove
    where hasSolution :: [(Index,Numbers)] -> Bool
	  hasSolution nl = let solved = solve False (constructTable nl)
			   in isSolution solved
	


