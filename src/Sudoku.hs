module Sudoku
  ( makeGameEnv
  , makeNewGame
  , makeGame
  ) where

import  Generator.GeneratorUtil  (GeneratorEnv(..), GeneratorStep, CellIndex, generatorSteps, removeCells)
import  Generator.FieldGenerator (runFieldGenerator, baseField)
import  Generator.GameGenerator  (generateGameField)
import  qualified Common as Com

import  Control.Monad.State      (State, evalState, get, put)
import  Control.Lens.Getter      ((^.))
import  System.Random            (randoms, randomRs, newStdGen)
import  qualified Data.Map.Strict as Map


dropRandomValue :: [a] -> [a]
dropRandomValue = drop 65

takeRandomValues :: [a] -> [a]
takeRandomValues = take 65

makeNewGame :: Com.Difficulties -> IO Com.GameEnv
makeNewGame diff = do
  gen <- newStdGen
  let randomSteps   = randoms gen
      cellsToRemove = randomRs (1, 81) gen
      genEnv        = GeneratorEnv randomSteps cellsToRemove
  return $ makeGame genEnv diff

makeGame :: GeneratorEnv -> Com.Difficulties -> Com.GameEnv
makeGame genEnv diff =
  let (restSteps, field)     = generateField (genEnv ^. generatorSteps)
      (restCells, gameField) = generateGame (genEnv ^. removeCells) field diff
  in makeGameEnv field gameField diff $ GeneratorEnv restSteps restCells


generateField :: [GeneratorStep] -> ([GeneratorStep], Com.Field)
generateField steps =
  let currSteps = takeRandomValues steps
      genField  = runFieldGenerator currSteps baseField
 in (dropRandomValue steps, genField)

generateGame :: [CellIndex] -> Com.Field -> Com.Difficulties -> ([CellIndex], Com.GameField)
generateGame cellsToRemove genField difficult =
  let genGame = generateGameField genField cellsToRemove difficult
  in (dropRandomValue cellsToRemove, genGame)

makeGameEnv :: Com.Field -> Com.GameField -> Com.Difficulties -> GeneratorEnv -> Com.GameEnv
makeGameEnv field gameField diff genEnv =
  let cells    = concat $ evalState (mapM collectCellCoord gameField) 0
      cellsMap = Map.fromList cells
  in Com.GameEnv field gameField cellsMap diff genEnv
  where
    collectCellCoord :: [Com.Cell] -> State Int [(Com.CellCoord, Com.CellValue)]
    collectCellCoord row = do
      rowNumber <- get

      let zippedCells = filter (\(_, cell) -> isOpenedCell cell) (zip [0..8] row)
          mappedRow   = map (\(column, (Com.Opened value)) -> ((column, rowNumber), value)) zippedCells

      put (rowNumber + 1)
      return mappedRow

    isOpenedCell :: Com.Cell -> Bool
    isOpenedCell Com.Closed     = False
    isOpenedCell (Com.Opened _) = True
