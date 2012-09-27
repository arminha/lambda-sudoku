--
-- Copyright (C) 2012  Armin Häberling
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Sudoku.Table
    (Numbers(One,Two,Three,Four,Five,Six,Seven,Eight,Nine)
    ,NumberSet
    ,Field
    ,Index
    ,Table
    ,constructTable
    ,emptyTable
    ,enter
    ,remove
    )

where

-- imports

import Data.Array
import Data.List

-- types

data Numbers = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
	     deriving (Show,Eq)

instance Enum Numbers where
    toEnum 1 = One
    toEnum 2 = Two
    toEnum 3 = Three
    toEnum 4 = Four
    toEnum 5 = Five
    toEnum 6 = Six
    toEnum 7 = Seven
    toEnum 8 = Eight
    toEnum 9 = Nine
    toEnum _ = error "Sudoku.Enum.Numbers.toEnum: bad argument"
    fromEnum One = 1
    fromEnum Two = 2
    fromEnum Three = 3
    fromEnum Four = 4
    fromEnum Five = 5
    fromEnum Six = 6
    fromEnum Seven = 7
    fromEnum Eight = 8
    fromEnum Nine = 9

type NumberSet = [Numbers]
type Field = Either Numbers NumberSet

type Index = (Int,Int)
type Table = Array Index Field


-- constructors and elemental functions

emptyField :: Field
emptyField = Right [One .. Nine]

removeF :: Numbers -> Field -> Field
removeF _ (Left m) = Left m
removeF n (Right ml) = Right (delete n ml)

set :: Numbers -> Field -> Field
set _ (Left m) = error ("Sudoku.set: field is already set to " ++ (show m))
set n (Right _) = Left n

emptyTable :: Table
emptyTable = listArray ((0,0),(8,8)) (replicate 81 emptyField)

constructTable :: [(Index,Numbers)] -> Table
constructTable = foldr (\(i,n) t -> enter n i t) emptyTable

assoc :: Index -> [Index]
assoc (x,y) = [(u,v) 
	       | u <- [0..8]
	      , v <- [0..8]
	      , (u /= x) || (v /= y)
	      , u == x
	       || v == y
	       || ((div x 3) == (div u 3) && (div y 3) == (div v 3)) ]

enter :: Numbers -> Index -> Table -> Table
enter n k t = t//((k,set n (t!k)):[(i,removeF n (t!i)) | i <- assoc k])

remove :: Numbers -> Index -> Table -> Table
remove n k t = t//[(k,removeF n (t!k))]
