module Lib
  (fps,cellSize,verticalCells,horizontalCells,screenDimensions    
  )
where
import GHC.Float (int2Float)

fps :: Int
fps = 15

cellSize :: Float
cellSize = 15

verticalCells :: Int
verticalCells = 60

horizontalCells :: Int
horizontalCells = 60

screenDimensions :: (Float, Float)
screenDimensions = (int2Float horizontalCells * cellSize, int2Float verticalCells * cellSize)

