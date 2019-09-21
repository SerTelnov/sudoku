module UI.FieldRender
  ( renderField
  ) where

import  qualified Common
import  UI.CoordinatesConverter (cellCoordToXY, cellSize, blockSize, 
                                  blockCoordToXY, buttonToXY, buttonSize)
import  UI.Util (Target (..), UIEnv (..), SudokuEnv (..), 
                  curTarget, uiEnv, gameEnv, countOfNumbers, wasMistake)

import  Control.Monad.Reader (ReaderT, runReaderT, ask)
import  Control.Monad.State (State, evalState, get, put)
import  Control.Lens.Getter ((^.))

import  qualified Data.Map.Strict as Map

import  qualified Graphics.Gloss.Data.Picture as Pic
import  Graphics.Gloss.Data.Color (Color, black, greyN, white, withBlue, red, light)

-- | State of cell
data CellState
  = CurrentTarget
  | SameNumberAsTarget
  | SameLogicArea
  | Simple
  deriving (Show, Eq)

isCurrentTarget :: Target -> Common.CellCoord -> Bool
isCurrentTarget NoTarget               = \_ -> False
isCurrentTarget (Target coord)         = (== coord)
isCurrentTarget (NumberTarget coord _) = (== coord)

isSameNumberAsTarger :: Target -> Common.CellValue -> Bool
isSameNumberAsTarger NoTarget               = \_ -> False
isSameNumberAsTarger (Target _)             = \_ -> False
isSameNumberAsTarger (NumberTarget _ value) = (== value)

isSameLogicArea :: Target -> Common.CellCoord -> Bool
isSameLogicArea NoTarget _               = False
isSameLogicArea (NumberTarget coord _) p = isSameLogicArea (Target coord) p
isSameLogicArea (Target (x, y)) (cx, cy) = x == cx || y == cy || isSameBlock
  where
    isSameBlock :: Bool
    isSameBlock =
      let xBlock  = getBlockNumber x
          cxBlock = getBlockNumber cx
          yBlock  = getBlockNumber y
          cyBlock = getBlockNumber cy
      in xBlock == cxBlock && yBlock == cyBlock

    getBlockNumber :: Int -> Int
    getBlockNumber i
      | i >= 0 && i < 3 = 0
      | i >= 3 && i < 6 = 1
      | otherwise       = 2


renderField :: SudokuEnv -> Pic.Picture
renderField env =
  let curField       = env ^. (gameEnv . Common.currentGameField)
      isMistakeMod   = env ^. (uiEnv . wasMistake)

      fieldPictures  = evalState (runReaderT (fieldToPictures curField) isMistakeMod) 0
      fieldPicture   = Pic.pictures fieldPictures
      buttonsPicture = renderButtons (env ^. uiEnv)
  in Pic.pictures $ fieldPicture : renderBlocks : buttonsPicture : []
  where
    fieldToPictures :: Common.GameField -> ReaderT Bool (State Int) [Pic.Picture]
    fieldToPictures [] = return []
    fieldToPictures (row : rest) = do
      isMistakeMod <- ask
      rowNum <- get

      let indexCells   = zip row [0 .. (length row - 1)]
          cellPictures = map (\(cell, column) ->
                          let (x, y) = cellCoordToXY (column, rowNum)
                              cellState = getCellState cell (column, rowNum)
                              cellColor = getCellColor cellState isMistakeMod
                          in Pic.translate x y (renderCell cell cellColor)) indexCells

      put (rowNum + 1)
      next <- fieldToPictures rest
      return $ cellPictures ++ next

    getCellState :: Common.Cell -> Common.CellCoord -> CellState
    getCellState cell currCoord =
      let target = env ^. (uiEnv . curTarget)
      in if isCurrentTarget target currCoord
        then CurrentTarget
        else case cell of
          Common.Closed       -> 
            if isSameLogicArea target currCoord
              then SameLogicArea
              else Simple
          Common.Opened value ->
            if isSameNumberAsTarger target value
              then SameNumberAsTarget
              else if isSameLogicArea target currCoord
                then SameLogicArea
                else Simple

    getCellColor :: CellState -> Bool -> Color
    getCellColor CurrentTarget False  = withBlue 0.75 white
    getCellColor CurrentTarget True   = light red
    getCellColor SameNumberAsTarget _ = withBlue 0.85 white
    getCellColor SameLogicArea      _ = withBlue 0.95 white
    getCellColor Simple             _ = greyN 0.95

    renderCell :: Common.Cell -> Color -> Pic.Picture
    renderCell cell cellColor =
      let cellPictures = case cell of
                          Common.Opened n -> [ Pic.scale 0.25 0.15 $ Pic.color black $ Pic.text $ show n ]
                          Common.Closed   -> []
      in Pic.Pictures $
        [ Pic.Color cellColor (Pic.rectangleSolid cellSize cellSize)
        , Pic.Color black     (Pic.rectangleWire cellSize cellSize) ]
        ++ cellPictures


renderBlocks :: Pic.Picture
renderBlocks =
  let blocks      = [0, 1, 2]
      blocksCoord = concat $ map (\i -> map (\j -> (i, j)) blocks) blocks
      blocksXY    = map blockCoordToXY blocksCoord
  in Pic.Pictures $ map (\(x, y) -> Pic.translate (x + 1) (y + 1) blockPicture) blocksXY
  where
    blockPicture :: Pic.Picture
    blockPicture = Pic.Color black (Pic.rectangleWire blockSize blockSize)


renderButtons :: UIEnv -> Pic.Picture
renderButtons env =
  let buttonsXY  = map buttonToXY [0..8]
      zipButtons = zip [1..9] buttonsXY
  in Pic.Pictures $ map (\(n, (x, y)) -> Pic.translate x y (buttonPicture n)) zipButtons
  where
    buttonPicture :: Common.CellValue -> Pic.Picture
    buttonPicture n =
      let isActiveButton = (Map.findWithDefault 0 n (env ^. countOfNumbers)) /= 9
      in case isActiveButton of
        True -> Pic.Pictures
          [ Pic.Color (greyN 0.95) (Pic.rectangleSolid buttonSize buttonSize)
          , Pic.Color black        (Pic.rectangleWire buttonSize buttonSize)
          , Pic.scale 0.25 0.15 $ Pic.color black $ Pic.text $ show n ]
        False -> Pic.Pictures
          [ Pic.Color (greyN 0.75) (Pic.rectangleSolid buttonSize buttonSize)
          , Pic.Color black        (Pic.rectangleWire buttonSize buttonSize) ]
