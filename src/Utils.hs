--
-- Copyright (C) 2012  Armin Häberling
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--

-- | Some utility functions.
module Utils where

import System.Random
import Data.List (sortBy)

groupAt :: Int -> [a] -> [[a]]
groupAt i l = f (splitAt i l)
    where f (x,[]) = [x]
	  f (x,y) = x:(groupAt i y)

-- | The 'firstElem' function takes a list of lists and returns the first non-empty list or an empty list if all list are empty.
firstElem :: [[a]] -> [a]
firstElem [] = []
firstElem (x:xs) = if null x then firstElem xs else x

-- | Takes a random generator and a list and returns the list elements in random order together with the new random generator
randOrder :: RandomGen g => g -- ^ random generator
	  -> [a] -- ^ input list
	  -> ([a],g)
randOrder gen lst = let fList = randFloatList (length lst) gen
			mySort (f1,_) (f2,_) = compare f1 f2
			randLst = snd (unzip (sortBy mySort (zip (fst fList) lst)))
		    in (randLst,snd fList)


-- | Returns a list of length n of random 'Float' values together with a new Random Generator
randFloatList :: RandomGen g => Int -- ^ number of Floats
	      -> g -- ^ random generator
	      -> ([Float],g)
randFloatList 0 gen = ([],gen)
randFloatList n gen = let newVal = random gen
			  recursive = randFloatList (n-1) (snd newVal)
		      in ((fst newVal):(fst recursive),snd recursive)

