module CellularAutomata.GameOfLife (GameOfLife (..)) where

import CellularAutomata (CellularAutomata (..))
import Data.Map.Strict (fromList)
import Graphics.Gloss (black, green)

-- https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
data GameOfLife = DeadGol | AliveGol deriving (Eq, Ord)

instance CellularAutomata GameOfLife where
  nextValue DeadGol = AliveGol
  nextValue AliveGol = DeadGol

  defaultOnBoard = DeadGol

  toColor AliveGol = green
  toColor DeadGol = black

  lookupNextStep =
    fromList
      [ (DeadGol, (deadTransformations, DeadGol)),
        (AliveGol, (aliveTransformations, DeadGol))
      ]
    where
      deadTransformations =
        fromList
          [(fromList [(AliveGol, 3), (DeadGol, 5)], AliveGol)]

      aliveTransformations =
        fromList
          [ (fromList [(AliveGol, 2), (DeadGol, 6)], AliveGol),
            (fromList [(AliveGol, 3), (DeadGol, 5)], AliveGol)
          ]
