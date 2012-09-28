--
-- Copyright (C) 2012  Armin Häberling
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module TestTables where

import Sudoku

-- test Tables

testTable :: Table
testTable = constructTable
	    [((0,2),Four) ,((0,4),Two) ,((0,5),One) ,((0,7),Seven)
	    ,((1,0),Five) ,((1,5),Six) ,((1,7),Eight)
	    ,((2,0),Three) ,((2,5),Nine) ,((2,7),Two)
	    ,((3,2),One)  ,((3,4),Eight) ,((3,6),Five)
	    ,((4,0),Six)  ,((4,2),Seven) ,((4,6),Nine) ,((4,8),Eight)
	    ,((5,2),Nine) ,((5,4),Three) ,((5,6),Four)
	    ,((6,1),Six) ,((6,3),Five) ,((6,8),One)
	    ,((7,1),Two) ,((7,3),One) ,((7,8),Three)
	    ,((8,1),Nine) ,((8,3),Seven) ,((8,4),Four) ,((8,6),Eight)
	    ]


testTable2 :: Table
testTable2 = constructTable
	     [((0,5),Two)
	     ,((1,2),Five) ,((1,5),Four)
	     ,((2,7),Six) ,((2,8),Seven)
	     ,((3,1),One)
	     ,((4,0),Seven)  ,((4,1),Six)
	     ,((5,6),Nine) ,((5,7),Eight)
	     ,((6,8),Six)
	     ,((7,2),Nine) ,((7,4),Four)
	     ,((8,0),Three) ,((8,2),Four) ,((8,4),Nine) ,((8,5),Five)
	     ]


testTable3 :: Table
testTable3 = constructTable 
	     [((0,0),Two),((0,4),Three),((0,8),Eight)
	     ,((1,3),Seven),((1,5),One)
	     ,((2,2),Five),((2,6),One)
	     ,((3,2),Nine),((3,6),Five)
	     ,((4,1),Six),((4,4),Nine),((4,7),Two)
	     ,((5,0),Three),((5,3),Four),((5,4),Two),((5,5),Six),((5,8),Nine)
	     ,((6,1),Three),((6,2),Two),((6,4),One),((6,6),Seven),((6,7),Four)
	     ,((7,4),Seven)
	     ,((8,3),Six),((8,5),Five)
	     ]

testTable4 :: Table
testTable4 = constructTable
             [((0,7),One),((0,8),Four)
             ,((1,0),Two),((1,3),Five),((1,6),Six)
             ,((2,0),Nine),((2,3),Three)
             ,((3,1),Five),((3,4),One),((3,7),Three)
             ,((4,1),Eight),((4,4),Three),((4,7),Seven)
             ,((5,1),Six),((5,4),Two),((5,7),Nine)
             ,((6,5),Eight),((6,8),Two)
             ,((7,2),Three),((7,5),Four),((7,8),One)
             ,((8,0),Five),((8,1),Seven)
             ]

testTable5 :: Table
testTable5 = constructTable
	     [((0,0),Three),((0,1),One),((0,3),Six)
	     ,((1,2),Two)
	     ,((2,1),Eight),((2,4),Nine),((2,6),Seven),((2,7),Six)
	     ,((3,5),Five)
	     ,((4,1),Six),((4,4),One),((4,7),Nine)
	     ,((5,3),Four)
	     ,((6,1),Nine),((6,2),Eight),((6,4),Six),((6,7),Three)
	     ,((7,6),Five)
	     ,((8,5),Seven),((8,7),Four),((8,8),Two)
	     ]

{-
3__ _5_ __1
_4_ ___ _8_
__2 ___ 9__

___ 5_3 ___
1__ _9_ __3
___ 8_7 ___

__8 ___ 2__
_7_ ___ _4_
6__ _3_ __5
-}
hardPuzzle :: Table
hardPuzzle = constructTable
        [((0,0), Three), ((0,4), Five), ((0,8), One)
        ,((1,1), Four), ((1,7),Eight)
        ,((2,2), Two), ((2, 6), Nine)
        ,((3,3), Five), ((3,5), Three)
        ,((4,0), One), ((4,4), Nine), ((4,8), Three)
        ,((5,3), Eight), ((5,5), Seven)
        ,((6,2), Eight), ((6,6), Two)
        ,((7,1), Seven), ((7,7), Four)
        ,((8,0), Six), ((8,4), Three), ((8,8), Five)
        ]


{-
_ 7 _ _ 1 _ _ 9 _
9 _ _ 8 _ _ _ _ 7
_ _ 3 _ _ _ _ _ 6
_ 4 _ _ _ 1 5 _ _
_ 3 _ _ _ _ _ 1 _
_ _ 2 7 _ _ _ 6 _
5 _ _ _ _ _ 6 _ _
6 _ _ _ _ 5 _ _ 2
_ 8 _ _ 2 _ _ 7 _
-}

{-
autosolve does not work with

_71 9_3 __6
___ _5_ 9__
_3_ ___ ___

___ __5 _2_
__3 _9_ _51
___ 3__ __8

___ 7__ __5
_25 4__ _6_
__9 _1_ 7__

and 

_1_ 4__ ___
___ ___ _9_
__8 _9_ 26_

___ 12_ _3_
_3_ ___ __7
__5 _8_ ___

5__ _6_ ___
_2_ 7__ ___
_8_ __5 614
-}
