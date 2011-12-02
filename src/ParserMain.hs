
module Main where

import System.Environment
import Parser

main =
    do args <- getArgs
       cprog <- parseFile (head args)
       putStrLn $ cProgramToString cprog
