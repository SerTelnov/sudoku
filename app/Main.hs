module Main where

import  Sudoku (runGame)
import  Common (Difficulties (..))

main :: IO ()
main = runGame Normal
