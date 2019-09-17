module UI.FieldRender
  ( renderField
  ) where

import  Common (Cell (..), GameField, CellCoord, CellValue)
import  Sudoku (GameEnv, GameEnv (..), currentGameField, numHolder)
import  UI.CoordinatesConverter (cellCoordToXY, cellSize, blockSize, blockCoordToXY, buttonToXY, buttonSize)
import  UI.Util (Target (..), UIEnv (..), SudokuEnv (..), curTarget, uiEnv, gameEnv, countOfNumbers)

import  Control.Monad.State (State, evalState, get, put)
import  Control.Lens.Getter ((^.))

import  qualified Data.Map.Strict as Map

import  qualified Graphics.Gloss.Data.Picture as Pic
import  Graphics.Gloss.Data.Color (black, greyN, white, withBlue)

-- | State of cell
data CellState
  = CurrentTarget
  | SameNumberAsTarget
  | SameLogicArea
  | Simple
  deriving (Show, Eq)

isCurrentTarget :: Target -> CellCoord -> Bool
isCurrentTarget NoTarget               = \_ -> False
isCurrentTarget (Target coord)         = (== coord)
isCurrentTarget (NumberTarget coord _) = (== coord)

isSameNumberAsTarger :: Target -> CellValue -> Bool
isSameNumberAsTarger NoTarget               = \_ -> False
isSameNumberAsTarger (Target _)             = \_ -> False
isSameNumberAsTarger (NumberTarget _ value) = (== value)

isSameLogicArea :: Target -> CellCoord -> Bool
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
  let curField       = env ^. (gameEnv . currentGameField)
      fieldPictures  = evalState (fieldToPictures curField) 0
      fieldPicture   = Pic.pictures fieldPictures
      buttonsPicture = renderButtons (env ^. uiEnv)
  in Pic.pictures $ fieldPicture : renderBlocks : buttonsPicture : []
  where
    fieldToPictures :: GameField -> State Int [Pic.Picture]
    fieldToPictures [] = return []
    fieldToPictures (row : rest) = do
      rowNum <- get

      let indexCells   = zip row [0 .. (length row - 1)]
          cellPictures = map (\(cell, column) ->
                          let (x, y) = cellCoordToXY (column, rowNum)
                              cellState = getCellState cell (column, rowNum)
                          in Pic.translate x y (renderCell cell cellState)) indexCells

      put (rowNum + 1)
      next <- fieldToPictures rest
      return $ cellPictures ++ next

    getCellState :: Cell -> CellCoord -> CellState
    getCellState cell currCoord =
      let target = env ^. (uiEnv . curTarget)
      in if isCurrentTarget target currCoord
        then CurrentTarget
        else case cell of
          Closed       -> 
            if isSameLogicArea target currCoord
              then SameLogicArea
              else Simple
          Opened value ->
            if isSameNumberAsTarger target value
              then SameNumberAsTarget
              else if isSameLogicArea target currCoord
                then SameLogicArea
                else Simple

    renderCell :: Cell -> CellState -> Pic.Picture
    renderCell cell cellState =
      let cellColor = case cellState of
                        CurrentTarget      -> withBlue 0.8  white
                        SameNumberAsTarget -> withBlue 0.9  white
                        SameLogicArea      -> withBlue 0.95 white
                        Simple             -> greyN 0.95
          cellPictures = case cell of
                          Opened n -> [ Pic.scale 0.25 0.15 $ Pic.color black $ Pic.text $ show n ]
                          Closed   -> []
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
    buttonPicture :: CellValue -> Pic.Picture
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