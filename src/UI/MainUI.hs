module UI.MainUI
  ( makeUI
  ) where

import  Common (Field, GameField, CellCoord, Cell (..), CellValue,
                OpenCellError (..), GameEnv, GameEnv (..), 
                builtField, currentGameField, numHolder)
import  EventHandler (openCell)
import  UI.CoordinatesConverter (xyToCellCoord, xyToButton)
import  UI.FieldRender (renderField)
import  UI.Util (Target (..), UIEnv (..), SudokuEnv (..), curTarget, uiEnv, gameEnv, countOfNumbers, numberOfMistakes)

import  Control.Monad.State  (State, execState, get, put)
import  Control.Lens.Getter ((^.))
import  Control.Lens.Setter ((.~), (%~), (+~))
import  Data.Char (isDigit, digitToInt)
import  Data.Function ((&))
import  qualified Data.Map.Strict as Map

import  Graphics.Gloss (play, Display ( InWindow ))
import  Graphics.Gloss.Data.Color (greyN, withBlue, white)
import  Graphics.Gloss.Data.Picture (Picture)
import  Graphics.Gloss.Interface.IO.Interact


makeUI :: GameEnv -> IO ()
makeUI newGameEnv =
  let newUIEnv = UIEnv NoTarget getCountOfNumber 0
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
    (InWindow "Sudoku" (470, 600) (5, 5))
    (greyN 0.9)
    50
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
    Nothing    -> tryOpenCell (xyToButton point) env
    Just coord ->
      let openedCell = Map.lookup coord (env ^. (gameEnv . numHolder))
          newTarget  = case openedCell of
            Nothing    -> Target coord
            Just value -> NumberTarget coord value
      in env & uiEnv . curTarget .~ newTarget
-- handleUIEvent (EventKey (Char key) Down (Modifiers { shift = Down }) _) env = 
--   case key of
--     'N' -> env %~ makeNewGame (handleEvent RestartGame)
--     'L' -> handleEvent (ChangeGameLevel) env
--     'H' -> handleEvent RestartGame
--     _   -> env
handleUIEvent (EventKey (Char key) _ _ _) env =
  tryOpenCell (getValueFromButton key) env
handleUIEvent _ env = env

tryOpenCell :: Maybe CellValue -> SudokuEnv -> SudokuEnv
tryOpenCell Nothing  env  = env
tryOpenCell (Just n) env  =
  let target = env ^. (uiEnv . curTarget)
  in case target of
    NoTarget         -> env
    NumberTarget _ _ -> env
    Target coord     -> 
      case openCell (env ^. gameEnv) coord n of
        Left WrongValue -> env
            & uiEnv . numberOfMistakes +~ 1
        Left  _                    -> env
        Right newGameEnv           -> env
            & gameEnv .~ newGameEnv
            & uiEnv . curTarget .~ NoTarget
            & uiEnv . countOfNumbers %~ (Map.update (\v -> Just (v + 1)) n)

getValueFromButton :: Char -> Maybe Int
getValueFromButton button =
  if isDigit button
    then Just $ digitToInt button
    else Nothing
