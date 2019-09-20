module Generator.GameGenerator
  ( CellIndex
  , generateGameField
  ) where

import  Common                  (Difficulties (..), Cell (..), GameField, Field)
import  Generator.GeneratorUtil (CellIndex)

import  Control.Monad.State     (State, evalState, get, put)
import  Data.Set                (Set, fromList, member)


generateGameField :: Field -> [CellIndex] -> Difficulties -> GameField
generateGameField field cellsToRemove difficulty =
  let cellsToRemoveSet = fromList $ takeCellsToRemove cellsToRemove difficulty
  in evalState (toGameField cellsToRemoveSet field) 0
  where
    cellsLimit :: Int
    cellsLimit = 81

    takeCellsToRemove :: [CellIndex] -> Difficulties -> [CellIndex]
    takeCellsToRemove toRemove diff =
      let removeCount = case diff of
                          Easy   -> cellsLimit - 45
                          Normal -> cellsLimit - 35
                          Hard   -> cellsLimit - 25
      in take removeCount toRemove

    toGameField :: Set CellIndex -> [[Int]] -> State Int GameField
    toGameField _ []             = return []
    toGameField set (row : rest) = do
      rowNumber <- get
      let gameRow = map (\cell ->
                          if (member (cell + rowNumber * 9) set)
                            then Closed
                            else Opened cell
                        ) row

      put (rowNumber + 1)
      restGame <- toGameField set rest
      return $ gameRow : restGame
