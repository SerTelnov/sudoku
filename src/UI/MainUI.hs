module UI.MainUI
  ( runUI
  ) where

import  Common (Field, GameField, CellCoord, Cell (..), CellValue,
                OpenCellError (..), GameEnv, GameEnv (..), NewGameOption (..),
                numHolder, level, genEnv)
import  EventHandler (openCell, restartGame)
import  UI.CoordinatesConverter (xyToCellCoord, xyToButton)
import  UI.FieldRender (renderField)
import  UI.InformationRender (renderInformation)
import  UI.Util (Target (..), UIEnv (..), SudokuEnv (..), curTarget, uiEnv, 
                  gameEnv, countOfNumbers, numberOfMistakes, wasMistake)

import  Control.Monad.State  (State, execState, get, put)
import  Control.Lens.Getter ((^.))
import  Control.Lens.Setter ((.~), (%~), (+~))
import  Data.Char (isDigit, digitToInt)
import  Data.Function ((&))
import  qualified Data.Map.Strict as Map

import  Graphics.Gloss (play, Display ( InWindow ))
import  Graphics.Gloss.Data.Color (greyN, withBlue, white)
import  Graphics.Gloss.Data.Picture (Picture ( Pictures ))
import  Graphics.Gloss.Interface.IO.Interact


runUI :: GameEnv -> IO ()
runUI = render . makeUI

makeUI :: GameEnv -> SudokuEnv
makeUI newGameEnv =
  let newUIEnv = UIEnv NoTarget getCountOfNumber 0 False
  in  SudokuEnv newUIEnv newGameEnv
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
    100
    env
    renderWorld
    handleUIEvent
    (\_ -> id)

renderWorld :: SudokuEnv -> Picture
renderWorld env = Pictures 
    [ renderField env
    , renderInformation env ]

handleUIEvent :: Event -> SudokuEnv -> SudokuEnv
handleUIEvent (EventKey (MouseButton LeftButton) Down _ point) env = 
  let maybeCell = xyToCellCoord point
  in case maybeCell of
    Nothing    -> tryOpenCell (xyToButton point) env
    Just coord ->
      let openedCell = Map.lookup coord (env ^. (gameEnv . numHolder))
          newTarget  = case openedCell of
            Nothing    -> Target coord
            Just value -> NumberTarget coord value
      in env 
        & uiEnv . curTarget .~ newTarget
        & uiEnv . wasMistake .~ False
handleUIEvent (EventKey (Char key) Down (Modifiers { shift = Down }) _) env = 
  case key of
    'N' -> restartUIGame env SameLevel
    'E' -> restartUIGame env PreviousLevel
    'H' -> restartUIGame env NextLevel
    _   -> env
handleUIEvent (EventKey (Char key) Down _ _) env =
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
            & uiEnv . wasMistake .~ True
        Left  _                    -> env
        Right newGameEnv           -> env
            & gameEnv .~ newGameEnv
            & uiEnv . curTarget .~ NoTarget
            & uiEnv . countOfNumbers %~ (Map.update (\v -> Just (v + 1)) n)
            & uiEnv . wasMistake .~ False

getValueFromButton :: Char -> Maybe Int
getValueFromButton button =
  if isDigit button
    then Just $ digitToInt button
    else Nothing


restartUIGame :: SudokuEnv -> NewGameOption -> SudokuEnv
restartUIGame env op =
  let currLevel  = env ^. gameEnv . level
      gameGen    = env ^. gameEnv . genEnv
      newGameEnv =  restartGame gameGen currLevel op
  in makeUI newGameEnv
