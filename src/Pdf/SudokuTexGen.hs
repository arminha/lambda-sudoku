--
-- Copyright (C) 2012  Armin Häberling
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--

-- | Create a Pdf file with Sudoku puzzles.
-- | Requires pdflatex and the sudoku package (see http://www.ctan.org/pkg/sudoku)
module Main where

import Sudoku
import Utils(groupAt)

import Data.Array

import System.Random
import System.Process
import System.Exit
import System.IO

main :: IO ()
main = 
    do
    t1 <- randomTable
    t2 <- randomTable
    createpdf $ texTable [t1,t2]

createpdf :: String -> IO ()
createpdf str = 
    do
    (inp,out,err,pid) <- runInteractiveProcess "pdflatex" ["-halt-on-error"] Nothing Nothing 
    hPutStr inp str
    hClose inp
    hGetContents out
    hGetContents err
    ec <- waitForProcess pid
    exitWith ec

randomTable :: IO Table
randomTable = 
    do
    r1 <- randomIO
    r2 <- randomIO
    return (constructTable $ newPuzzle r1 r2)

texTable :: [Table] -> String
texTable tl = texheader ++ (concat $ map table2tex tl) ++ texfooter
    where texheader = "\\documentclass[a4paper]{article}\n" ++
                      "\\usepackage{sudoku}\n" ++
                      "\\begin{document}\n"
          texfooter = "\\end{document}"
          table2tex t = let fields      = groupAt 9 $ map printField (elems t)
                            tableString = concat $ map (\x -> concat x ++ "|.\n") fields
                	    in "\\begin{sudoku}\n" ++ tableString ++ "\\end{sudoku}\n"
          printField (Left n) = "|" ++ toString n
          printField (Right _) = "| "
