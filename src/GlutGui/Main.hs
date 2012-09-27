--
-- Copyright (C) 2012  Armin Häberling
-- GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Main where

import GlutGui.GLGui

import System.Environment(getArgs)

main :: IO ()
main =
    do 
    args <- getArgs
    runGui "GLSudoku" args
