module CellularAutomata (CellularAutomata (..)) where

import Data.Map.Strict (Map)
import Graphics.Gloss (Color)

type DefaultTransformation a = a

type NewCell a = a

type CurrentCellState a = a

type Neighbors a = Map a Int

type Transformation a = Map (Neighbors a) (NewCell a)

class CellularAutomata a where
  lookupNextStep :: Map (CurrentCellState a) (Transformation a, DefaultTransformation a)

  defaultOnBoard :: a

  toColor :: a -> Color

  nextValue :: a -> a
