module CellularAutomata.DayAndNight (DayAndNight) where

import CellularAutomata (CellularAutomata (..))
import Data.Map.Strict (fromList)
import Graphics.Gloss (blue, yellow)

-- http://www.tip.net.au/~dbell/articles/DayNight.zip
data DayAndNight = AliveDN | DeadDN deriving (Eq, Ord)

instance CellularAutomata DayAndNight where
  toColor AliveDN = yellow
  toColor DeadDN = blue
  lookupNextStep =
    fromList
      [ (DeadDN, (deadTransformations, DeadDN)),
        (AliveDN, (aliveTransformations, DeadDN))
      ]
    where
      deadTransformations =
        fromList
          [ (fromList [(AliveDN, 3), (DeadDN, 5)], AliveDN),
            (fromList [(AliveDN, 6), (DeadDN, 2)], AliveDN),
            (fromList [(AliveDN, 7), (DeadDN, 1)], AliveDN),
            (fromList [(AliveDN, 8)], AliveDN)
          ]
      aliveTransformations =
        fromList
          [ (fromList [(AliveDN, 3), (DeadDN, 5)], AliveDN),
            (fromList [(AliveDN, 4), (DeadDN, 4)], AliveDN),
            (fromList [(AliveDN, 6), (DeadDN, 2)], AliveDN),
            (fromList [(AliveDN, 7), (DeadDN, 1)], AliveDN),
            (fromList [(AliveDN, 8)], AliveDN)
          ]

  defaultOnBoard = DeadDN
  nextValue AliveDN = DeadDN
  nextValue DeadDN = AliveDN
