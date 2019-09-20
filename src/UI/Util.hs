{-# LANGUAGE TemplateHaskell #-}

module UI.Util
  ( Target (..)
  , UIEnv (..)
  , SudokuEnv (..)

  , curTarget
  , uiEnv
  , gameEnv
  , countOfNumbers
  , numberOfMistakes
  , wasMistake
  ) where

import  Common (CellCoord, CellValue, GameEnv (..))
import  Control.Lens.Combinators (makeLenses)

import  qualified Data.Map.Strict as Map

-- | User target
data Target
  = NoTarget                         -- ^ no click
  | Target CellCoord                 -- ^ click on closed cell
  | NumberTarget CellCoord CellValue -- ^ click on opened cell
  deriving (Eq, Show)

data UIEnv = UIEnv
  { _curTarget :: Target
  , _countOfNumbers :: Map.Map CellValue Int
  , _numberOfMistakes :: Int
  , _wasMistake :: Bool
  } deriving (Show)

makeLenses ''UIEnv

data SudokuEnv = SudokuEnv
  { _uiEnv :: UIEnv
  , _gameEnv :: GameEnv 
  } deriving (Show)

makeLenses ''SudokuEnv
