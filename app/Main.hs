module Main (main) where

import Graphics.Gloss.Interface.Pure.Game (Display (FullScreen), Event, Picture (), black, blank, play)

fps :: Int
fps = 60

newtype State = State {running :: Bool}

initialState :: State
initialState = State {running = False}

view :: State -> Picture
view _ = blank

event :: Event -> State -> State
event _ = id

periodic :: Float -> State -> State
periodic _ = id

main :: IO ()
main = play FullScreen black fps initialState view event periodic
