
module Main where

import System.Exit
import System.Environment

main :: IO ()
main = getArgs >>= print . haqify . head

haqify s = "Haq! " ++ s