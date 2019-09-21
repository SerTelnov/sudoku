module EventHandler
  ( openCell
  , restartGame
  ) where

import  Generator.GeneratorUtil (GeneratorEnv (..))
import  Sudoku                  (makeGame)
import  qualified Common as Com

import  Control.Lens.Getter ((^.))
import  Control.Lens.Setter ((.~), (%~))
import  Data.Function       ((&))

import  qualified Data.Map.Strict as Map


restartGame :: GeneratorEnv -> Com.Difficulties -> Com.NewGameOption -> Com.GameEnv
restartGame genEnv diff option = makeGame genEnv $ getNewDiff diff option
  where
    getNewDiff :: Com.Difficulties -> Com.NewGameOption -> Com.Difficulties
    getNewDiff diff' Com.SameLevel         = diff'
    getNewDiff Com.Easy  Com.PreviousLevel = Com.Easy
    getNewDiff diff' Com.PreviousLevel     = toEnum $ fromEnum diff' - 1
    getNewDiff Com.Hard  Com.NextLevel     = Com.Hard
    getNewDiff diff' Com.NextLevel         = toEnum $ fromEnum diff' + 1

openCell :: Com.GameEnv -> Com.CellCoord -> Com.CellValue -> Either Com.OpenCellError Com.GameEnv
openCell env coord@(columnIndex, rowIndex) value =
  let cell        = ((env ^. Com.currentGameField) !! rowIndex) !! columnIndex
      actualValue = ((env ^. Com.builtField) !! rowIndex) !! columnIndex
  in case cell of
    Com.Opened _ -> Left Com.AlreadyOpen
    Com.Closed   -> if value == actualValue
                  then Right updEnv
                  else Left Com.WrongValue
  where
    updEnv :: Com.GameEnv
    updEnv =
      let newField = updGameField (env ^. Com.currentGameField)
      in env
         & Com.currentGameField .~ newField
         & Com.numHolder %~ (Map.insert coord value)

    updGameField :: Com.GameField -> Com.GameField
    updGameField field =
      let (a, b)   = splitAt rowIndex field
          currRow  = head b
          (ai, bi) = splitAt columnIndex currRow
          updRow   = ai ++ (Com.Opened value : (tail bi))
      in a ++ (updRow : (tail b))
