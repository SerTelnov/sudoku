module UI.MainUI
  ( makeUI
  ) where

import  Common (Field, GameField, CellCoord, Cell (..), SudokuEvent (..), CellValue)
import  Sudoku (GameEnv, GameEnv (..), builtField, currentGameField, numHolder)
import  EventHandler (handleEvent)
import  UI.CoordinatesConverter (xyToCellCoord, xyToButton)
import  UI.FieldRender (renderField)
import  UI.Util (Target (..), UIEnv (..), SudokuEnv (..), curTarget, uiEnv, gameEnv, countOfNumbers)

import  Control.Monad.State  (State, execState, get, put)
import  Control.Lens.Getter ((^.))
import  Control.Lens.Setter ((.~), (%~))
import  Data.Function ((&))
import  qualified Data.Map.Strict as Map

import  Graphics.Gloss (play, Display ( InWindow ))
import  Graphics.Gloss.Data.Color (greyN, withBlue, white)
import  Graphics.Gloss.Data.Picture (Picture)
import  Graphics.Gloss.Interface.IO.Interact


makeUI :: GameEnv -> IO ()
makeUI newGameEnv =
  let newUIEnv = UIEnv NoTarget getCountOfNumber
      env      = SudokuEnv newUIEnv newGameEnv
  in render env
  where
    getCountOfNumber :: Map.Map CellValue Int
    getCountOfNumber =
      let openedCellValues = Map.elems $ newGameEnv ^. numHolder
      in execState (mapM collectMap openedCellValues) Map.empty

    collectMap :: CellValue -> State (Map.Map CellValue Int) ()
    collectMap currentValue = do
      currMap <- get

      let count  = Map.findWithDefault 0 currentValue currMap
          newMap = Map.insert currentValue (count + 1) currMap
      
      put newMap
      return ()

render :: SudokuEnv -> IO ()
render env =
  play
    (InWindow "Sudoku" (500, 600) (5, 5))
    (greyN 0.9)
    100
    env
    renderWorld
    handleUIEvent
    (\_ -> id)

renderWorld :: SudokuEnv -> Picture
renderWorld env = renderField env

handleUIEvent :: Event -> SudokuEnv -> SudokuEnv
handleUIEvent (EventKey (MouseButton LeftButton) _ _ point) env = 
  let maybeCell = xyToCellCoord point
  in case maybeCell of
    Nothing    -> openCell (xyToButton point) env
    Just coord ->
      let openedCell = Map.lookup coord (env ^. (gameEnv . numHolder))
          newTarget  = case openedCell of
            Nothing    -> Target coord
            Just value -> NumberTarget coord value
      in env & uiEnv . curTarget .~ newTarget
handleUIEvent (EventKey key@(SpecialKey _) _ _ _) env = 
  openCell (getKeyEvent key) env
handleUIEvent _ env = env

openCell :: Maybe CellValue -> SudokuEnv -> SudokuEnv
openCell Nothing  env  = env
openCell (Just n) env  =
  let target = env ^. (uiEnv . curTarget)
  in case target of
    NoTarget         -> env
    NumberTarget _ _ -> env
    Target coord     -> 
      case handleEvent (OpenCell coord n) (env ^. gameEnv) of
        Left _           -> env
        Right newGameEnv -> env
            & gameEnv .~ newGameEnv
            & uiEnv . curTarget .~ NoTarget
            & uiEnv . countOfNumbers %~ (Map.update (\v -> Just (v + 1)) n)

getKeyEvent :: Key -> Maybe Int
getKeyEvent (SpecialKey key) = 
  let number = case key of
                KeyF1 -> 1
                KeyF2 -> 2
                KeyF3 -> 3
                KeyF4 -> 4
                KeyF5 -> 5
                KeyF6 -> 6
                KeyF7 -> 7
                KeyF8 -> 8
                KeyF9 -> 9
                _     -> -1
  in if number > 0
      then Just number
      else Nothing
getKeyEvent _ = Nothing
