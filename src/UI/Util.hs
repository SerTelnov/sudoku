{-# LANGUAGE TemplateHaskell #-}

module UI.Util
  ( Target (..)
  , UIEnv (..)
  , SudokuEnv (..)

  , curTarget
  , uiEnv
  , gameEnv
  ) where

import  Common (CellCoord, CellValue)
import  Sudoku (GameEnv (..))

import  Control.Lens.Combinators (makeLenses)

-- | User target
data Target
  = NoTarget -- ^ no click (start of game)
  | Target CellCoord -- ^ click on closed cell
  | NumberTarget CellCoord CellValue -- ^ click on opened cell
  deriving (Eq, Show)

data UIEnv = UIEnv
  { _curTarget :: Target
  } deriving (Show)

makeLenses ''UIEnv

data SudokuEnv = SudokuEnv
  { _uiEnv :: UIEnv
  , _gameEnv :: GameEnv 
  } deriving (Show)

makeLenses ''SudokuEnv
