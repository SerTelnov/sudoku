{-# LANGUAGE TemplateHaskell #-}

module Sudoku
  ( generateGame
  , makeGameEnv

  , GameEnv (..)
  , builtField
  , currentGameField
  , numHolder
  ) where

import  Common                   (Difficulties (..), Cell (..), GameField, Field, CellCoord, CellValue)
import  Generator.FieldGenerator (runFieldGenerator, baseField)
import  Generator.GameGenerator  (generateGameField)

import  Control.Monad.State      (State, evalState, get, put)
import  Control.Lens.Combinators (makeLenses)
import  System.Random            (StdGen, randoms, randomRs, newStdGen)
import  qualified Data.Map.Strict as Map


-- | Game enviroment
data GameEnv = GameEnv
  { _builtField :: Field -- ^ generated field
  , _currentGameField :: GameField -- ^ current game state
  , _numHolder :: Map.Map CellCoord CellValue -- ^ mapper for opened cells
  } deriving (Show, Eq)

makeLenses ''GameEnv


generateRandomField :: StdGen -> IO Field
generateRandomField gen = do
  let randomSteps = take 100 $ randoms gen
  return $ runFieldGenerator randomSteps baseField

generateGame :: Difficulties -> IO (Field, GameField)
generateGame difficult = do
  gen <- newStdGen
  genField <- generateRandomField gen

  let cellsToRemove = randomRs (1, 81) gen
      genGame       = generateGameField genField cellsToRemove difficult

  return $ (genField, genGame)

makeGameEnv :: Field -> GameField -> GameEnv
makeGameEnv field gameField =
  let cells    = concat $ evalState (mapM collectCellCoord gameField) 0
      cellsMap = Map.fromList cells
  in GameEnv field gameField cellsMap
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
