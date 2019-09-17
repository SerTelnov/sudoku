module Main where

import  UI.MainUI (makeUI)
import  Common (Difficulties (..))
import  Sudoku (generateGame, makeGameEnv)

main :: IO ()
main = do
  (field, gameField) <- generateGame Hard
  let env = makeGameEnv field gameField

  makeUI env
