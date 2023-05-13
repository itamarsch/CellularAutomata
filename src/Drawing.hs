module Drawing (boardLines, boardSquares) where

import CellularAutomata (CellularAutomata (..))
import Control.Monad (join)
import Graphics.Gloss (Color, Picture (..))
import Graphics.Gloss.Data.Picture (rectangleSolid)
import Grid (Grid)
import Lib (cellSize, screenDimensions)

boardLines :: Color -> [Picture]
boardLines c =
  let rows = (\t -> [(0.0, t), (fst screenDimensions, t)]) <$> [0.0, cellSize .. snd screenDimensions]
      columns = (\t -> [(t, 0.0), (t, snd screenDimensions)]) <$> [0.0, cellSize .. fst screenDimensions]
   in Color c . Line <$> (rows <> columns)

boardSquares :: (CellularAutomata a) => Grid a -> [Picture]
boardSquares grid =
  let cellCoordList = (+ (cellSize / 2)) <$> [0.0, cellSize ..]

      drawRect x y = Translate x y $ join rectangleSolid $ cellSize - 1 -- Subtracting one to show lines
      firstDim :: (CellularAutomata a) => Float -> [a] -> [Picture]
      firstDim i = zipWith (secondDim i) cellCoordList

      secondDim :: (CellularAutomata a) => Float -> Float -> a -> Picture
      secondDim x y a = Color (toColor a) $ drawRect x y
   in concat (zipWith firstDim cellCoordList grid)
