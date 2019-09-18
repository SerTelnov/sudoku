module EventHandler
  ( openCell
  ) where

import  Common (Cell (..), GameField, CellCoord, CellValue, OpenCellError (..), 
                GameEnv (..), currentGameField, numHolder, builtField)

import  Control.Lens.Getter ((^.))
import  Control.Lens.Setter ((.~), (%~))
import  Data.Function ((&))

import  qualified Data.Map.Strict as Map


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
