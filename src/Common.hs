module Common
  ( Difficulties (..)
  , Cell (..)
  , GameField
  , Field
  , CellCoord
  , CellValue
  , SudokuEvent (..)
  ) where

-- | Game Difficulties
data Difficulties
  = Easy
  | Normal
  | Hard
  deriving (Show, Eq, Enum)

-- | Value in the Cell
-- value from 1 to 9
type CellValue = Int

-- | Field cell
data Cell
  = Opened CellValue -- ^ opened cell
  | Closed -- ^ closed cell
  deriving Eq

instance Show Cell where
    show :: Cell -> String
    show (Opened i) = show i
    show Closed     = "."


type Field = [[CellValue]]
type GameField = [[Cell]]

-- | Cell coordinates
-- values from 0 to 8
-- first  is number of column
-- second is number of row
type CellCoord = (Int, Int)


data SudokuEvent
  = OpenCell CellCoord CellValue
