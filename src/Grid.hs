{-# LANGUAGE LambdaCase #-}

module Grid (nextGrid, countNeighbors, Grid, State (..)) where

import CellularAutomata (CellularAutomata (..))
import Data.Foldable (foldl')
import Data.Map.Strict (Map, alter, empty, (!), (!?))
import Data.Maybe (fromMaybe)

type Grid a = [[a]]

data State a = State
  { running :: Bool,
    getGrid :: Grid a
  }

nextGrid :: (CellularAutomata a, Ord a) => State a -> State a
nextGrid state =
  let singleDoubleTuple a b c = ((a, b), c)
      enumeratedGrid = (\(i, xs) -> zipWith3 singleDoubleTuple (repeat i) [0 :: Int ..] xs) <$> zip [0 :: Int ..] (getGrid state)
      newGrid =
        (fmap . fmap)
          ( \(coords, value) ->
              let neighbors = countNeighbors coords $ getGrid state
                  (lookupTable, defaultNext) = lookupNextStep ! value
               in fromMaybe defaultNext (lookupTable !? neighbors)
          )
          enumeratedGrid
   in state {getGrid = newGrid}

countNeighbors :: (Ord a) => (Int, Int) -> Grid a -> Map a Int
countNeighbors (x, y) board =
  let neighborCoords =
        [ let (neighborX, neighborY) = (x + x', y + y')
           in ( circularValue
                  neighborX
                  0
                  (length board - 1),
                circularValue
                  neighborY
                  0
                  (length (head board) - 1)
              )
          | x' <- [-1 .. 1 :: Int],
            y' <- [-1 .. 1 :: Int],
            x' /= 0 || y' /= 0
        ]

      circularValue n small large
        | n < small = large
        | n > large = small
        | otherwise = n

      insertToMapOrIncrement :: (Ord a) => Map a Int -> a -> Map a Int
      insertToMapOrIncrement =
        flip
          ( alter
              ( \case
                  Just a -> Just $ a + 1
                  Nothing -> Just 1
              )
          )
   in foldl' insertToMapOrIncrement empty $ (\(neighborX, neighborY) -> board !! neighborX !! neighborY) <$> neighborCoords
