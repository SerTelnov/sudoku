module EventHandler
  ( openCell
  , restartGame
  ) where

import  Common        (Cell (..), GameField, CellCoord, CellValue, OpenCellError (..), NewGameOption (..),
                        GameEnv (..), Difficulties (..), currentGameField, numHolder, builtField)
import  Generator.GeneratorUtil (GeneratorEnv (..))
import  Sudoku                  (makeGame)
import  System.Random           (StdGen)

import  Control.Lens.Getter ((^.))
import  Control.Lens.Setter ((.~), (%~))
import  Data.Function ((&))

import  qualified Data.Map.Strict as Map


restartGame :: GeneratorEnv -> Difficulties -> NewGameOption -> GameEnv
restartGame genEnv diff option = makeGame genEnv $ getNewDiff diff option
  where
    getNewDiff :: Difficulties -> NewGameOption -> Difficulties
    getNewDiff diff' SameLevel     = diff'
    getNewDiff Easy  PreviousLevel = Easy
    getNewDiff diff' PreviousLevel = toEnum $ fromEnum diff' - 1
    getNewDiff Hard  NextLevel     = Hard
    getNewDiff diff' NextLevel     = toEnum $ fromEnum diff' + 1

openCell :: GameEnv -> CellCoord -> CellValue -> Either OpenCellError GameEnv
openCell env coord@(columnIndex, rowIndex) value =
  let cell        = ((env ^. currentGameField) !! rowIndex) !! columnIndex
      actualValue = ((env ^. builtField) !! rowIndex) !! columnIndex
  in case cell of
    Opened _ -> Left AlreadyOpen
    Closed   -> if value == actualValue
                  then Right updEnv
                  else Left WrongValue
  where
    updEnv :: GameEnv
    updEnv =
      let newField = updGameField (env ^. currentGameField)
      in env
         & currentGameField .~ newField
         & numHolder %~ (Map.insert coord value)

    updGameField :: GameField -> GameField
    updGameField field =
      let (a, b)   = splitAt rowIndex field
          currRow  = head b
          (ai, bi) = splitAt columnIndex currRow
          updRow   = ai ++ (Opened value : (tail bi))
      in a ++ (updRow : (tail b))
