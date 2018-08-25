module Main where

import Graphics.Gloss

screenWidth :: Int
screenWidth = 480

screenHeight :: Int
screenHeight = 480

screenOffset :: Int
screenOffset = 100

window :: Display
window = InWindow "Simon" (480, 480) (100, 100)

backgroundColor :: Color
backgroundColor = makeColorI 51 51 51 255

yellowOff = color $ makeColorI 254 210 48 255
blueOff = color $ makeColorI 20 136 189 255
redOff = color $ makeColorI 194 10 41 255
greenOff = color $ makeColorI 23 162 106 255

radius = 150.0
thickness = 100.0

greenArc  = thickArc 90.0 180.0 radius thickness
redArc    = thickArc 0.0  90.0 radius thickness
yellowArc = thickArc 180.0 270.0 radius thickness
blueArc   = thickArc 270.0 359.9 radius thickness

gamePicture :: Picture
gamePicture = pictures [ greenOff $ greenArc
                       , redOff $ redArc
                       , yellowOff $ yellowArc
                       , blueOff $ blueArc
                       ]

main :: IO ()
main = display window backgroundColor gamePicture
