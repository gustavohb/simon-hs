module Main where

import Graphics.Gloss

import System.Random

import Game
import Logic
import Rendering

window :: Display
window = InWindow "Simon" (screenWidth, screenHeight) (screenOffset, screenOffset)

backgroundColor :: Color
backgroundColor = makeColorI 51 51 51 255

main :: IO ()
main = do
    gen <- newStdGen
    let rndColorSeq  = genColorSeq gen maxSeqLen
    simulate window backgroundColor fps initialState {colorSeq = rndColorSeq} gameAsPicture update
