module UI.InformationRender 
  ( renderInformation
  ) where

import  Common (Cell (..), GameField, CellCoord, CellValue,
                GameEnv, GameEnv (..), currentGameField, numHolder)
import  UI.CoordinatesConverter (cellCoordToXY, cellSize, blockSize, blockCoordToXY, buttonToXY, buttonSize)
import  UI.Util (Target (..), UIEnv (..), SudokuEnv (..), curTarget, uiEnv, gameEnv, countOfNumbers)

import  Control.Monad.State (State, evalState, get, put)
import  Control.Lens.Getter ((^.))

import  qualified Data.Map.Strict as Map

import  qualified Graphics.Gloss.Data.Picture as Pic
import  Graphics.Gloss.Data.Color (black, greyN, white, withBlue)
    

renderInformation :: SudokuEnv -> Pic.Picture
renderInformation env = undefined
