{-# LANGUAGE TemplateHaskell #-}

module Common
  ( Difficulties (..)
  , Cell (..)
  , GameField
  , Field
  , CellCoord
  , CellValue
  , OpenCellError (..)
  , NewGameOption (..)

  , GameEnv (..)
  , builtField
  , currentGameField
  , numHolder
  , level
  , genEnv
  ) where

import  Generator.GeneratorUtil  (GeneratorEnv)

import  Control.Lens.Combinators (makeLenses)
import  qualified Data.Map.Strict as Map


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
  | Closed           -- ^ closed cell
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


-- | Game enviroment
data GameEnv = GameEnv
  { _builtField :: Field                      -- ^ generated field
  , _currentGameField :: GameField            -- ^ current game state
  , _numHolder :: Map.Map CellCoord CellValue -- ^ mapper for opened cells
  , _level :: Difficulties                    -- ^ game level
  , _genEnv :: GeneratorEnv                   -- ^ random generator
  } deriving (Show, Eq)


makeLenses ''GameEnv


-- | Options for new game
data NewGameOption
  = SameLevel    -- ^ Same level
  | NextLevel    -- ^ More difficult level
  | PreviousLevel -- ^ Easier level
  deriving (Show, Eq, Enum)

-- | OpenCell's unsuccessful Options
data OpenCellError
  = WrongValue  -- ^ Cell had different value
  | AlreadyOpen -- ^ Cell was opened
  deriving (Show, Eq)
