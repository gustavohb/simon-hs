module Main where

import Graphics.Gloss

import Game
import Logic
import Rendering

window :: Display
window = InWindow "Simon" (screenWidth, screenHeight) (screenOffset, screenOffset)

backgroundColor :: Color
backgroundColor = makeColorI 51 51 51 255

main :: IO ()
main = simulate window backgroundColor fps initialState gameAsPicture update
