module Main where

import Graphics.Gloss

import System.Random

import Game
import Logic
import Rendering

window :: Display
window = InWindow "Simon" (screenWidth, screenHeight) (screenOffset, screenOffset)

main :: IO ()
main = do
    gen <- newStdGen
    let rndColorSeq  = genColorSeq gen maxSeqLen
    play window backgroundColor fps initialState {colorSeq = rndColorSeq} renderState handleInput update
