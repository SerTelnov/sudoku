module Sudoku
  ( makeGameEnv
  , makeNewGame
  , makeGame
  ) where

import  Common  (Difficulties (..), Cell (..), GameField, Field, CellCoord,
                  CellValue, NewGameOption (..), GameEnv (..))
import  Generator.GeneratorUtil  (GeneratorEnv(..), GeneratorStep, CellIndex, generatorSteps, removeCells)
import  Generator.FieldGenerator (runFieldGenerator, baseField)
import  Generator.GameGenerator  (generateGameField)

import  Control.Monad.State      (State, evalState, get, put)
import  Control.Lens.Combinators (makeLenses)
import  Control.Lens.Getter      ((^.))
import  System.Random            (StdGen, randoms, randomRs, newStdGen)
import  qualified Data.Map.Strict as Map


dropRandomValue :: [a] -> [a]
dropRandomValue = drop 100

makeNewGame :: Difficulties -> IO GameEnv
makeNewGame diff = do
  gen <- newStdGen
  let randomSteps   = randoms gen
      cellsToRemove = randomRs (1, 81) gen
      genEnv        = GeneratorEnv randomSteps cellsToRemove
  return $ makeGame genEnv diff

makeGame :: GeneratorEnv -> Difficulties -> GameEnv
makeGame genEnv diff =
  let (restSteps, field)     = generateField (genEnv ^. generatorSteps)
      (restCells, gameField) = generateGame (genEnv ^. removeCells) field diff
  in makeGameEnv field gameField diff $ GeneratorEnv restSteps restCells


generateField :: [GeneratorStep] -> ([GeneratorStep], Field)
generateField steps =
  let currSteps = take 100 steps
      genField  = runFieldGenerator currSteps baseField
 in (dropRandomValue steps, genField)

generateGame :: [CellIndex] -> Field -> Difficulties -> ([CellIndex], GameField)
generateGame cellsToRemove genField difficult =
  let genGame = generateGameField genField cellsToRemove difficult
  in (dropRandomValue cellsToRemove, genGame)

makeGameEnv :: Field -> GameField -> Difficulties -> GeneratorEnv -> GameEnv
makeGameEnv field gameField diff genEnv =
  let cells    = concat $ evalState (mapM collectCellCoord gameField) 0
      cellsMap = Map.fromList cells
  in GameEnv field gameField cellsMap diff genEnv
  where
    collectCellCoord :: [Cell] -> State Int [(CellCoord, CellValue)]
    collectCellCoord row = do
      rowNumber <- get

      let zippedCells = filter (\(_, cell) -> isOpenedCell cell) (zip [0..8] row)
          mappedRow   = map (\(column, (Opened value)) -> ((column, rowNumber), value)) zippedCells

      put (rowNumber + 1)
      return mappedRow

    isOpenedCell :: Cell -> Bool
    isOpenedCell Closed     = False
    isOpenedCell (Opened _) = True
