module CellularAutomata.Seeds (Seeds) where

import CellularAutomata (CellularAutomata (..))
import Data.Map.Strict (empty, fromList)
import Graphics.Gloss (black, white)

-- https://en.wikipedia.org/wiki/Seeds_(cellular_automaton)
data Seeds = AlivesS | DeadS deriving (Ord, Eq)

instance CellularAutomata Seeds where
  defaultOnBoard = DeadS

  toColor DeadS = black
  toColor AlivesS = white

  nextValue DeadS = AlivesS
  nextValue AlivesS = DeadS

  lookupNextStep =
    fromList
      [ (AlivesS, (empty, DeadS)),
        (DeadS, (deadCellTransformations, DeadS))
      ]
    where
      deadCellTransformations =
        fromList
          [ (fromList [(AlivesS, 2), (DeadS, 6)], AlivesS)
          ]
