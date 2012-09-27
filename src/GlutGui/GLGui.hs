--
-- Copyright (C) 2012  Armin Häberling
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--

-- | A Gui for the "Sudoku" backend.
module GlutGui.GLGui
    (runGui)
where

import Graphics.UI.GLUT hiding (Index)

import System.Exit(exitWith,ExitCode(ExitSuccess))
import Data.IORef
import System.Random(randomIO)
import Data.Char(isDigit,digitToInt,intToDigit)
import Data.Array(assocs)
import Data.List((\\))

import Sudoku

-- * Types

data States = 
   States 
   { freezeNum :: IORef [(Index,Numbers)] -- ^ freezed numbers
   , otherNum :: IORef [(Index,Numbers)] -- ^ other numbers
   , showMarks :: IORef Bool -- ^ show marks
   , showFalse :: IORef Bool -- ^ mark false numbers on the field
   , selected :: IORef Index -- ^ selected field
   , showCredit :: IORef Bool -- ^ show credits
   , backtrack :: IORef Bool -- ^ use backtracking to solve puzzles
   }

-- * Initialize and Main Functions

-- | Create a new 'States' object and initialze the values
initStates :: IO States
initStates = 
    do
    freezeNumRef <- newIORef []
    otherNumRef <- newIORef []
    showMarksRef <- newIORef False
    showFalseRef <- newIORef False
    selectedRef <- newIORef (0,0)
    showCreditRef <- newIORef False
    backtrackRef <- newIORef True
    return (States
	    freezeNumRef 
	    otherNumRef 
	    showMarksRef 
            showFalseRef
	    selectedRef
	    showCreditRef
	    backtrackRef
	   )

-- | The standard menu
createMenu :: States -> Menu
createMenu states =
    Menu [MenuEntry "[alt]+[n] New Puzzle" (menuNewPuzzle states)
	 ,MenuEntry "[alt]+[e] Empty Puzzle" (menuEmptyPuzzle states)
	 ,MenuEntry "" (return ())
	 ,MenuEntry "[b] Back" (menuBack states)
	 ,MenuEntry "[r] Revert" (menuRevert states)
	 ,MenuEntry "[alt]+[f] Freeze" (menuFreeze states)	 
	 ,MenuEntry "" (return ())
	 ,MenuEntry "[m] Toggle Hints" (menuToggleMarks states)
         ,MenuEntry "[f] Show false on/off" (menuToggleFalse states)
	 ,MenuEntry "[alt]+[s] Solve Puzzle" (menuSolvePuzzle states)
	 ,MenuEntry "[alt]+[a] Auto Solve" (autoSolvePuzzle states)
	 ,MenuEntry "" (return ())
	 ,MenuEntry "[c] Credits" (menuShowCredit states)
	 ,MenuEntry "[q] Quit" (exitWith ExitSuccess)
	 ]

-- | The credits menu
createCreditMenu :: States -> Menu
createCreditMenu states = 
    Menu [MenuEntry "[Esc] End" (menuRemoveCredit states)
	 ,MenuEntry "[q] Quit" (exitWith ExitSuccess)
	 ]

-- | Start the Gui. Giving the window title and the command line arguments for the glut library.
runGui :: String -- ^ window title
       -> [String] -- ^ command line arguments
       -> IO ()
runGui prName args = 
    do
    nonGlutArgs <- initialize prName args
    initialDisplayMode $= [RGBMode,DoubleBuffered,WithDepthBuffer]
    initialWindowSize $= Size _initWindowWidth _initWindowHeight
    window <- createWindow prName
    depthFunc $= Just Lequal
    blend $= Enabled
    blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
    -- antialiased Lines
    lineSmooth $= Disabled
    (hint LineSmooth) $= Nicest
    states <- initStates
    displayCallback $= (display states)
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse states)
    attachMenu RightButton (createMenu states)
    mainLoop


-- * Constants

_initWindowHeight, _initWindowWidth :: GLsizei
_initWindowHeight = 500
_initWindowWidth = 500

_top,_left,_right,_bottom :: GLfloat
_top = 1.0
_left = -1.0
_right = 1.0
_bottom = -1.0

_border :: GLfloat
_border = 0.05

_tbTop,_tbLeft,_tbRight,_tbBottom :: GLfloat
_tbTop = _top - _border
_tbLeft = _left + _border
_tbRight = _right - _border
_tbBottom = _bottom + _border

_fHeight,_fWidth :: GLfloat
_fHeight = (_tbTop - _tbBottom) / 9
_fWidth = (_tbRight - _tbLeft) / 9

_bgLevel,_selLevel,_tableLevel,_numLevel,_fgLevel :: GLfloat
_bgLevel = -2.0
_selLevel = -1.0
_tableLevel = 0.0
_numLevel = 1.0
_fgLevel = 2.0

_antialiasedLines :: Bool
_antialiasedLines = True

-- * Menu Actions

-- | Create a new sudoku puzzle
menuNewPuzzle :: States -> IO ()
menuNewPuzzle states =
    let freeze = freezeNum states
	other = otherNum states
    in do
       g1 <- randomIO
       g2 <- randomIO
       freeze $= newPuzzle g1 g2
       other $= []
       postRedisplay Nothing

-- | Create an empty Sudoku puzzle
menuEmptyPuzzle :: States -> IO ()
menuEmptyPuzzle states =
    let freeze = freezeNum states
	other = otherNum states
    in do
       freeze $= []
       other $= []
       postRedisplay Nothing

-- | Solve the given Sudoku puzzle
menuSolvePuzzle :: States -> IO ()
menuSolvePuzzle states =
    do
    freeze <- get $ freezeNum states
    other <- get $ otherNum states
    bt <- get $ backtrack states
    let tabElem = assocs (solve bt $ constructTable $ freeze ++ other)
	el = [(ind,num) | (ind,Left num) <- tabElem]
    (otherNum states) $= ((el \\ (freeze ++ other)) ++ other)
    postRedisplay Nothing

-- | Freeze all values in the table
menuFreeze :: States -> IO ()
menuFreeze states = 
    let freeze = freezeNum states
	other = otherNum states
    in do
       otherVal <- get other
       freeze $~ ((++) otherVal)
       other $= []
       postRedisplay Nothing

-- | Delete the last set value on the table
menuBack :: States -> IO ()
menuBack states = 
    let other = otherNum states
    in do
       otherVal <- get other
       other $~ (if null otherVal then id else tail)
       postRedisplay Nothing

-- | Clean the puzzle
menuRevert :: States -> IO ()
menuRevert states =
    let other = otherNum states
    in do
       other $= []
       postRedisplay Nothing

menuToggleMarks :: States -> IO ()
menuToggleMarks states =
    do
    (showMarks states) $~ not
    postRedisplay Nothing

menuToggleFalse :: States -> IO ()
menuToggleFalse states =
    do
    (showFalse states) $~ not
    postRedisplay Nothing

-- | Show the credits screen
menuShowCredit :: States -> IO ()
menuShowCredit states =
    do
    (showCredit states) $~ not
    keyboardMouseCallback $= Just (kbmCallback states)
    attachMenu RightButton (createCreditMenu states)
    postRedisplay Nothing

-- | Exit the credits screen
menuRemoveCredit :: States -> IO ()
menuRemoveCredit states = 
    do
    (showCredit states) $~ not
    keyboardMouseCallback $= Just (keyboardMouse states)
    attachMenu RightButton (createMenu states)
    postRedisplay Nothing

-- | Solve the puzzle step by step automatically
autoSolvePuzzle :: States -> IO ()
autoSolvePuzzle states =
    do
    freeze <- get $ freezeNum states
    other <- get $ otherNum states
    let tab = constructTable $ freeze ++ other
    case (nextStep tab) of
         Just (Enter num ind) -> do 
				 (otherNum states) $~ (\x -> ((ind,num):x))
				 postRedisplay Nothing 
				 addTimerCallback 1000 (autoSolvePuzzle states)
	 _ -> return ()


-- * Callback Functions

-- ** Display Callback

-- | display callback
display :: States -> DisplayCallback
display states =
    do
    clear [ColorBuffer,DepthBuffer]
    displayBackground
    ind <- get $ selected states
    displaySelection ind
    sf <- get $ showFalse states
    if sf
       then displayFalse states
       else return ()
    displayTable
    lineAntialiasing <- get lineSmooth
    if _antialiasedLines
       then lineSmooth $= Enabled 
       else lineSmooth $= Disabled
    displayNumbers states
    credit <- get $ showCredit states
    if credit 
       then displayCredit
       else return ()
    lineSmooth $= lineAntialiasing
    swapBuffers

-- | display the background
displayBackground :: IO ()
displayBackground =
    do
    currentColor $= Color4 1.0 1.0 1.0 1.0
    preservingMatrix $ do
		       loadIdentity
		       (_,(Size vx vy)) <- get viewport
		       translate (Vector3 0.0 0.0 _bgLevel)
		       let ratio = (fromIntegral vx)/(fromIntegral vy)
			   sv = if ratio >= 1
				   then (ratio,1.0)
				   else (1.0,(1/ratio))
		       scale (fst sv) (snd sv) (1.0::GLfloat)
		       renderAs Quads [(_left,_top)
				      ,(_right,_top)
				      ,(_right,_bottom)
				      ,(_left,_bottom)
				      ]

-- | display the current selection
displaySelection :: Index -> IO ()
displaySelection ind =
    do
--    currentColor $= Color4 0.2 0.55 0.9 1.0
    currentColor $= Color4 0.1 0.1 1.0 0.8
    preservingMatrix $ do
		       loadIdentity
		       translate (Vector3 
				  (_tbLeft + ((fromIntegral (fst ind)) * _fWidth)) 
				  (_tbTop - ((fromIntegral (snd ind)) * _fHeight)) 
				  _selLevel)
		       renderAs Quads [(0.0,0.0)
				      ,(_fWidth,0.0)
				      ,(_fWidth,- _fHeight)
				      ,(0.0,- _fHeight)
				      ]

markField :: Index -> Color4 GLfloat -> IO ()
markField ind markcolor =
    do
    currentColor $= markcolor
    preservingMatrix $ do
		       loadIdentity
		       translate (Vector3 
				  (_tbLeft + ((fromIntegral (fst ind)) * _fWidth)) 
				  (_tbTop - ((fromIntegral (snd ind)) * _fHeight)) 
				  _selLevel)
		       renderAs Quads [(0.0,0.0)
				      ,(_fWidth,0.0)
				      ,(_fWidth,- _fHeight)
				      ,(0.0,- _fHeight)
				      ]

-- | mark false numbers
displayFalse :: States -> IO ()
displayFalse states =
    do
    let markcolor = Color4 0.7 0.1 0.1 0.7
    freeze <- get $ freezeNum states
    other <- get $ otherNum states
    let solution = assocs (solve False $ constructTable freeze)
        err = [ind | (ind,num) <- other, (ind2,num2) <- solution, ind == ind2, (Left num) /= num2]
    mapM_ (\i -> markField i markcolor) err


-- | display the table borders
displayTable :: IO ()
displayTable =
    do
    currentColor $= Color4 0.0 0.0 0.0 1.0
    preservingMatrix $ do
		       loadIdentity
		       translate (Vector3 0.0  _tbTop _tableLevel)
		       mapM_ (uncurry (renderLineX _tbLeft _tbRight)) 
				 (zip (map ((-_fHeight)*) [0.0,1.0 .. 9.0]) thicknessList)
		       loadIdentity
		       translate (Vector3 _tbLeft 0.0 _tableLevel)
		       mapM_ (uncurry (renderLineY _tbBottom _tbTop)) 
				 (zip (map (_fWidth*) [0.0,1.0 .. 9.0]) thicknessList)
    where renderLineX ax bx y thickness =
	      do
	      lineWidth $= thickness
	      renderAs Lines [(ax,y),(bx,y)]
	  renderLineY ay by x thickness =
	      do
	      lineWidth $= thickness
	      renderAs Lines [(x,ay),(x,by)]
	  thicknessList = [3,1,1,3,1,1,3,1,1,3]

-- | display the numbers
displayNumbers :: States -> IO ()
displayNumbers states =
    do
    lineWidth $= 2
    currentColor $= Color4 0.0 0.0 0.0 1.0
    freeze <- get (freezeNum states)
    mapM_ displayNum freeze
    currentColor $= Color4 1.0 0.0 0.0 1.0
    other <- get (otherNum states)
    mapM_ displayNum other
    marks <- get (showMarks states)
    if (marks)
       then let elems = assocs (constructTable (freeze ++ other))
		dl = [(ind,nl) | (ind,Right nl) <- elems]
	    in do
	       currentColor $= Color4 0.0 1.0 0.0 1.0
	       mapM_ displayNumList dl
       else return ()

-- | display a single number in the given field
displayNum :: (Index,Numbers) -> IO ()
displayNum ((x,y),num) = 
    preservingMatrix $ do
		       loadIdentity
		       translate (Vector3 
				  (_tbLeft + ((fromIntegral x) * _fWidth))
				  (_tbTop - ((fromIntegral (y+1)) * _fHeight)) 
				  _numLevel)
		       let scFac = (1/119.05) * 0.9
		       scale (scFac * _fWidth) (scFac * _fHeight) (1.0 :: GLfloat)
		       translate (Vector3 10.0 15.0 (0.0 :: GLfloat))
		       renderString MonoRoman [intToDigit $ fromEnum num]

-- | display a number list in the given field
displayNumList :: (Index,NumberSet) -> IO ()
displayNumList ((x,y),nl) =
    preservingMatrix $ do
		       mapM_ renderNum nl
    where scFac = (1/119.05) * 0.25
	  renderNum num = 
	      do loadIdentity
		 translate (Vector3 
			    (_tbLeft + ((fromIntegral x) + (fromIntegral (((fromEnum num) -1) `mod` 3)) / 3) * _fWidth)
			    (_tbTop - ((fromIntegral (y+1)) - (fromIntegral (2-(((fromEnum num) -1) `div` 3))) / 3) * _fHeight)
			    _numLevel)
		 scale (scFac * _fWidth) (scFac * _fHeight) (1.0 :: GLfloat)
		 translate (Vector3 15.0 25.0 (0.0 :: GLfloat))
		 renderString MonoRoman [intToDigit $ fromEnum num]

-- | display the credits screen
displayCredit :: IO ()
displayCredit =
    do    
    currentColor $= Color4 1.0 1.0 1.0 0.7
    preservingMatrix $ do
		       loadIdentity
		       translate (Vector3 0.0 0.0 _fgLevel)
		       renderAs Quads [(_left,_top)
				      ,(_right,_top)
				      ,(_right,_bottom)
				      ,(_left,_bottom)
				      ]
    let string0 = "GLSudoku"
	string1 = "created by"
	string2 = "Armin Haeberling"
	string3 = "armin.aha@gmail.com"
	scFac = (1/119.05) * (_right - _left) * 0.08
    currentColor $= Color4 0.0 0.0 0.0 1.0
    preservingMatrix $ do
		       loadIdentity 
		       translate (Vector3 0.0 (0.3) (_fgLevel + 0.1))
		       lineWidth $= 3.0
		       preservingMatrix $ renderCenter Roman (scFac*1.5) string0
		       translate (Vector3 0.0 (-0.3) (0.0 :: GLfloat))
		       lineWidth $= 2.0
		       preservingMatrix $ renderCenter Roman scFac string1
		       translate (Vector3 0.0 (-0.2) (0.0 :: GLfloat))
		       preservingMatrix $ renderCenter Roman scFac string2
		       translate (Vector3 0.0 (-0.2) (0.0 :: GLfloat))
		       preservingMatrix $ renderCenter Roman (scFac * 0.7) string3				 
    where renderCenter :: Font a => a -> GLfloat -> String -> IO ()
	  renderCenter font scaleFac str =
	      do
	      width <- stringWidth font str
	      translate (Vector3 (- (fromIntegral width) * scaleFac * 0.5) 0.0 0.0)
	      scale scaleFac scaleFac (1.0 :: GLfloat)
	      renderString font str

-- ** Reshape Callback

-- | reshape callback
reshape :: ReshapeCallback
reshape size@(Size x y) = 
    do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    let f = fromRational . toRational
	ratio = (fromIntegral x)/(fromIntegral y)
    if ratio >= 1
       then ortho (f (ratio*_left)) (f (ratio*_right)) (f _bottom) (f _top) (-3.0) 3.0
       else ortho (f _left) (f _right) (f (_bottom/ratio)) (f (_top/ratio)) (-3.0) 3.0
    matrixMode $= Modelview 0
    

-- ** Keyboard and Mouse Callback

-- | standard keyboard mouse callback
keyboardMouse :: States -> KeyboardMouseCallback
keyboardMouse states (Char c) Down modifier _
    | isDigit c && c /= '0' = do
			      ind <- get $ selected states
			      --(freezeNum states) $~ (\x -> [el | el <- x, fst el /= ind])
			      -- check if cursor field is fixed
			      freezed <- get $ freezeNum states
			      if null [el | el <- freezed, fst el == ind] 
				 then (otherNum states) $~ (\x -> ((ind,toEnum $ digitToInt c):[el | el <- x, fst el /= ind]))
				 else return ()
			      postRedisplay Nothing
    | c == 'q' = exitWith ExitSuccess
    | c == '\t' = do
		  (x,y) <- get $ selected states
		  if (shift modifier) == Down 
		     then if x == 0
			  then (selected states) $= (8,(y-1) `mod` 9)
			  else (selected states) $= (x-1,y)
		     else if x == 8
			  then (selected states) $= (0,(y+1) `mod` 9)
			  else (selected states) $= (x+1,y)
		  postRedisplay Nothing
    | c == ' ' || c == '\DEL' = do
				ind <- get $ selected states
				--(freezeNum states) $~ (\x -> [el | el <- x, fst el /= ind])
				(otherNum states) $~ (\x -> [el | el <- x, fst el /= ind])
				postRedisplay Nothing
    | c == 'm' = menuToggleMarks states
    | c == 'f' && alt modifier == Up = menuToggleFalse states
    | c == 'b' = menuBack states
    | c == 'r' = menuRevert states
    | c == 'f' && alt modifier == Down = menuFreeze states
    | c == 'n' && alt modifier == Down = menuNewPuzzle states
    | c == 'e' && alt modifier == Down = menuEmptyPuzzle states
    | c == 's' && alt modifier == Down = menuSolvePuzzle states
    | c == 'a' && alt modifier == Down = autoSolvePuzzle states
    | c == 'c' = menuShowCredit states
    | c == 'x' = (backtrack states) $~ not
    | otherwise = return ()
keyboardMouse states (MouseButton b) Down _ (Position px py)
    | b == LeftButton = do
			(Size ww wh) <- get windowSize
			let x = _left + (_right - _left) * ((fromIntegral px) / (fromIntegral ww))
			    y = _top - (_top - _bottom) * ((fromIntegral py) / (fromIntegral wh))
			if (x >= _tbLeft && x <= _tbRight && y <= _tbTop && y >= _tbBottom)
			   then do
				(selected states) $= (truncate ((x - _tbLeft) / _fWidth)
						     ,truncate ((_tbTop - y) / _fHeight))
				postRedisplay Nothing
			   else return ()
    | otherwise = return ()
keyboardMouse states (SpecialKey sk) Down _ _
    | sk == KeyUp = do
		    (selected states) $~ (\(x,y) -> (x,max (y-1) 0))
		    postRedisplay Nothing
    | sk == KeyDown = do
		      (selected states) $~ (\(x,y) -> (x,min (y+1) 8))
		      postRedisplay Nothing
    | sk == KeyLeft = do
		      (selected states) $~ (\(x,y) -> (max (x-1) 0,y))
		      postRedisplay Nothing
    | sk == KeyRight = do
		       (selected states) $~ (\(x,y) -> (min (x+1) 8,y))
		       postRedisplay Nothing
    | otherwise = return ()
keyboardMouse _ _ Up _ _ = return ()


-- | keyboard mouse callback for the credits screen
kbmCallback :: States -> KeyboardMouseCallback
kbmCallback states key keyState _ _ =
    case key of 
    (Char 'q') -> exitWith ExitSuccess
    _ -> if keyState == Down
	 then menuRemoveCredit states
	 else return ()

-- * Render Functions

-- | render primitive
renderAs :: PrimitiveMode -> [(GLfloat,GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure$makeVertexes ps
    where makeVertexes = mapM_ (\(x,y) -> vertex$Vertex2 x y)

