module UI.CoordinatesConverter
  ( blockCoordToXY
  , cellCoordToXY
  , xyToCellCoord
  , buttonToXY
  , xyToButton

  , blockSize
  , cellSize
  , buttonSize

  , startXPoint
  , startYPoint
  ) where

import  Common (CellCoord, CellValue)

cellSize, blockSize, startYPoint, startXPoint :: Float
cellSize = 45.0
blockSize = cellSize * 3
startYPoint = 170.0
startXPoint = -175.0

buttonStartXPoint, buttonStartYPoint, buttonSize :: Float
buttonStartXPoint = -200.0
buttonStartYPoint = -250.0
buttonSize = 50.0

blockCoordToXY :: (Int, Int) -> (Float, Float)
blockCoordToXY (bx, by) =
  let x = startXPoint + blockSize * (realToFrac bx) + cellSize
      y = startYPoint - blockSize * (realToFrac by) - cellSize
  in (x, y)

cellCoordToXY :: CellCoord -> (Float, Float)
cellCoordToXY (cx, cy) =
  let x = startXPoint + cellSize * (realToFrac cx)
      y = startYPoint - cellSize * (realToFrac cy)
  in (x, y)  

xyToCellCoord :: (Float, Float) -> Maybe CellCoord
xyToCellCoord (x, y) =
  let cx = (+ 8)    <$> round $ (x + startXPoint) / cellSize
      cy = (* (-1)) <$> round $ (y - startYPoint) / cellSize
  in if (cx < 0 || cx > 8 || cy < 0 || cy > 8)
    then Nothing
    else Just (cx, cy)

buttonToXY :: Int -> (Float, Float)
buttonToXY n =
  let x = buttonStartXPoint + buttonSize * (realToFrac n)
  in (x, buttonStartYPoint)

xyToButton :: (Float, Float) -> Maybe CellValue
xyToButton (x, y) =
  let n = (+ 9) <$> round $ (x + buttonStartXPoint) / buttonSize
  in if n > 0 && n < 10 && isButtonBlock
     then Just n
     else Nothing
  where
    isButtonBlock :: Bool
    isButtonBlock = (round $ (y - buttonStartYPoint) / buttonSize) == 0
