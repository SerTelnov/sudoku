module Main where

import  Sudoku    (makeNewGame)
import  Common    (Difficulties (..))
import  UI.MainUI (runUI)


main :: IO ()
main = do
  env <- makeNewGame Normal
  runUI env
