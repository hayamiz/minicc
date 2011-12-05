
module Main where

import System.Environment
import System.Exit
import Parser
import Semantic

main =
    do args <- getArgs
       cprog <- parseFile (head args)
       errs <- return $ doSemCheck cprog
       handleErrors errs

handleErrors :: [SemError] -> IO ()
handleErrors errs =
    if null errs then
        exitSuccess
    else
        do mapM_ (putStrLn . show) errs
           exitFailure