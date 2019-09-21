module UI.InformationRender 
  ( renderInformation
  ) where

import  Common                  (level)
import  UI.CoordinatesConverter (startXPoint, startYPoint)
import  UI.Util                 (SudokuEnv (..), uiEnv, gameEnv, numberOfMistakes)

import  Control.Lens.Getter ((^.))

import  Graphics.Gloss.Data.Color (black)
import  qualified Graphics.Gloss.Data.Picture as Pic


xPoint, mistakesYPoint, levelYPoint :: Float
xPoint         = startXPoint - 40.0
mistakesYPoint = startYPoint + 100.0
levelYPoint    = mistakesYPoint - 50.0


renderInformation :: SudokuEnv -> Pic.Picture
renderInformation env =
  let mistakes  = env ^. (uiEnv . numberOfMistakes)
      currLevel = env ^. (gameEnv . level)
  in Pic.Pictures 
      [ translateText mistakesYPoint $ "Mistakes: " ++ (show mistakes)
      , translateText levelYPoint    $ "Level: "    ++ (show currLevel) ]

translateText :: Float -> String -> Pic.Picture
translateText yPoint text =
  Pic.translate xPoint yPoint $ textToPicture text

textToPicture :: String -> Pic.Picture
textToPicture text = Pic.scale 0.25 0.15 $ Pic.color black $ Pic.text text
