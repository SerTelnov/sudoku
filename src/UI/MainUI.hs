module UI.MainUI
  ( makeUI
  ) where

import  Common (Field, GameField, CellCoord, Cell (..))
import  Sudoku (GameEnv, GameEnv (..), builtField, currentGameField, numHolder)
import  UI.CoordinatesConverter (xyToCellCoord)
import  UI.FieldRender (renderField)
import  UI.Util (Target (..), UIEnv (..), SudokuEnv (..), curTarget, uiEnv, gameEnv)

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
  let newUIEnv = UIEnv NoTarget
      env      = SudokuEnv newUIEnv newGameEnv
  in render env

render :: SudokuEnv -> IO ()
render env =
  play
    (InWindow "Sudoku" (500, 600) (5, 5))
    (greyN 0.9)
    100
    env
    renderWorld
    handleEvent
    (\_ -> id)

renderWorld :: SudokuEnv -> Picture
renderWorld env = renderField env

handleEvent :: Event -> SudokuEnv -> SudokuEnv
handleEvent (EventKey (MouseButton LeftButton) _ _ point) env = 
  let maybeCell = xyToCellCoord point
  in case maybeCell of
    Nothing    -> env
    Just coord ->
      let openedCell = Map.lookup coord (env ^. (gameEnv . numHolder))
          newTarget  = case openedCell of
            Nothing    -> Target coord
            Just value -> NumberTarget coord value
      in env & uiEnv . curTarget .~ newTarget
handleEvent (EventKey key@(SpecialKey _) _ _ _) env =
  let target = env ^. (uiEnv . curTarget)
  in case target of
    NoTarget         -> env
    NumberTarget _ _ -> env
    Target coord     -> let number = getKeyEvent key
                        in  openCell env number coord
handleEvent _ env = env

openCell :: SudokuEnv -> Maybe Int -> CellCoord -> SudokuEnv
openCell env Nothing _ = env
openCell env (Just n) (j, i) =
  let cell      = ((env ^. (gameEnv . currentGameField)) !! i) !! j
      valueCell = ((env ^. (gameEnv . builtField)) !! i) !! j
  in case cell of
    Opened _ -> env
    Closed   -> if (n == valueCell)
                  then updEnv
                  else env
  where
    updEnv :: SudokuEnv
    updEnv =
      let newField = updGameField (env ^. (gameEnv . currentGameField))
      in env
         & gameEnv . currentGameField .~ newField
         & gameEnv . numHolder %~ (Map.insert (j, i) n)
         & uiEnv . curTarget .~ NoTarget

    updGameField :: GameField -> GameField
    updGameField field =
      let (a, b)   = splitAt i field
          row      = head b
          (ai, bi) = splitAt j row
          updRow   = ai ++ (Opened n : (tail bi))
      in a ++ (updRow : (tail b))

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
