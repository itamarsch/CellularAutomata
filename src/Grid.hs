{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

enumerate2d :: forall a. [[a]] -> [[((Int, Int), a)]]
enumerate2d grid =
  let enumerated1d :: [(Int, [a])]
      enumerated1d = zip [0 :: Int ..] grid

      doubleSingleTuple a1 b c = ((a1, b), c)
      enumerate1dto2d :: Int -> [a] -> [((Int, Int), a)]
      enumerate1dto2d i = zipWith3 doubleSingleTuple (repeat i) [0 :: Int ..]
   in uncurry enumerate1dto2d <$> enumerated1d

nextGrid :: forall a. (CellularAutomata a, Ord a) => State a -> State a
nextGrid state =
  let enumeratedGrid = enumerate2d $ getGrid state

      newGrid :: [[a]]
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
        [ ( circularValue (x + dx) 0 (length board - 1),
            circularValue (y + dy) 0 (length (head board) - 1)
          )
          | dx <- [-1 .. 1 :: Int],
            dy <- [-1 .. 1 :: Int],
            dx /= 0 || dy /= 0
        ]

      circularValue n bottom top
        | n < bottom = top
        | n > top = bottom
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
