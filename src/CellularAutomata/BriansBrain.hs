module CellularAutomata.BriansBrain (BriansBrain) where

import CellularAutomata (CellularAutomata (..))
import Data.Map.Strict (empty, fromList)
import Graphics.Gloss (black, blue, green)

-- https://en.wikipedia.org/wiki/Brian%27s_Brain
data BriansBrain = DeadBB | DyingBB | AliveBB deriving (Eq, Ord, Show)

instance CellularAutomata BriansBrain where
  nextValue DeadBB = AliveBB
  nextValue AliveBB = DyingBB
  nextValue DyingBB = DeadBB
  defaultOnBoard = DeadBB

  toColor DeadBB = black
  toColor DyingBB = blue
  toColor AliveBB = green

  lookupNextStep =
    fromList
      [ (AliveBB, (empty, DyingBB)),
        (DyingBB, (empty, DeadBB)),
        (DeadBB, (deadTransformations, DeadBB))
      ]
    where
      deadTransformations =
        fromList
          ( [(fromList [(DeadBB, n), (AliveBB, 2), (DyingBB, 6 - n)], AliveBB) | n <- [1 .. 5]]
              <> pure (fromList [(AliveBB, 2), (DyingBB, 6)], AliveBB)
              <> pure (fromList [(AliveBB, 2), (DeadBB, 6)], AliveBB)
          )
