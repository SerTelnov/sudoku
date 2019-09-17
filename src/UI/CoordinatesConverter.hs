module UI.CoordinatesConverter
  ( blockCoordToXY
  , cellCoordToXY
  , xyToCellCoord

  , blockSize
  , cellSize
  ) where

import  Common (CellCoord)

cellSize, blockSize, startYPoint, startXPoint :: Float
cellSize = 45.0
blockSize = cellSize * 3
startYPoint = 270.0
startXPoint = -175.0


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
