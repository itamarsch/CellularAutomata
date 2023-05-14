module Main (main) where

import CellularAutomata (CellularAutomata (..))
import CellularAutomata.Seeds (Seeds)
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Index (modifyAt)
import Drawing (boardLines, boardSquares)
import GHC.Float (float2Int)
import Graphics.Gloss (Picture (Pictures, Translate), black, dark, green)
import Graphics.Gloss.Interface.Pure.Game (Display (InWindow), Event (EventKey), Key (Char, MouseButton, SpecialKey), KeyState (Down), MouseButton (LeftButton), SpecialKey (KeyCtrlL, KeyEnter), play)
import Grid (State (..), nextGrid)
import Lib (cellSize, fps, horizontalCells, screenDimensions, verticalCells)

initialState :: (CellularAutomata a) => State a
initialState = State {running = False, getGrid = replicate horizontalCells $ replicate verticalCells defaultOnBoard}

view :: (CellularAutomata a) => State a -> Picture
view state =
  Translate (-fst screenDimensions / 2) (-snd screenDimensions / 2) $
    Pictures (boardLines (dark $ dark green) <> boardSquares (getGrid state))

event :: (Ord a, CellularAutomata a) => Event -> State a -> State a
event (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) state@(State _ grid) =
  let (x, y) = (mouseX + (fst screenDimensions / 2), mouseY + (snd screenDimensions / 2))
   in state
        { getGrid =
            modifyAt (float2Int $ x / cellSize) (modifyAt (float2Int $ y / cellSize) nextValue) grid
        }
event (EventKey (SpecialKey KeyEnter) Down _ _) state = state {running = not $ running state}
event (EventKey (SpecialKey KeyCtrlL) Down _ _) state = state {getGrid = getGrid $ nextGrid state}
event (EventKey (Char 'q') Down _ _) _ = initialState
event _ state = state

periodic :: (Ord a, CellularAutomata a) => Float -> State a -> State a
periodic _ state
  | running state = nextGrid state
  | otherwise = state

main :: IO ()
main = play (InWindow "Cellular" (both float2Int screenDimensions) (0, 0)) black fps (initialState :: State Seeds) view event periodic

both :: (a -> b) -> (a, a) -> (b, b)
both = join bimap
