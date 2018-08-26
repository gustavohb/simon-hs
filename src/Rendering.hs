module Rendering where

import Graphics.Gloss

import Game

screenWidth :: Int
screenWidth = 480

screenHeight :: Int
screenHeight = 480

screenOffset :: Int
screenOffset = 100

fps :: Int
fps = 60

yellowOn = color $ makeColorI 255 253 56 255
yellowOff = color $ makeColorI 254 210 48 255

blueOn = color $ makeColorI 36 185 252 255
blueOff = color $ makeColorI 20 136 189 255

redOn = color $ makeColorI 252 13 27 255
redOff = color $ makeColorI 194 10 41 255

greenOn = color $ makeColorI 11 215 131 255
greenOff = color $ makeColorI 23 162 106 255

radius = 150.0
thickness = 100.0

greenArc  = thickArc 90.0 180.0 radius thickness
redArc    = thickArc 0.0  90.0 radius thickness
yellowArc = thickArc 180.0 270.0 radius thickness
blueArc   = thickArc 270.0 359.9 radius thickness

data ButtonsPictures  = ButtonsPictures
  { greenButton :: Picture
  , redButton :: Picture
  , blueButton :: Picture
  , yellowButton :: Picture }

buttonsOff =  ButtonsPictures { greenButton = greenOff $ greenArc
                             , redButton = redOff $ redArc
                             , blueButton = blueOff $ blueArc
                             , yellowButton = yellowOff $ yellowArc }

gameAsPicture :: GameState -> Picture
gameAsPicture gs = case colorOn gs of
                        Just Green -> (buttonsAsPicture buttonsOff {greenButton = greenOn $ greenArc})
                        Just Red -> (buttonsAsPicture buttonsOff {redButton = redOn $ redArc})
                        Just Blue -> (buttonsAsPicture buttonsOff {blueButton = blueOn $ blueArc})
                        Just Yellow -> (buttonsAsPicture buttonsOff {yellowButton = yellowOn $ yellowArc})
                        Nothing -> (buttonsAsPicture buttonsOff)

buttonsAsPicture :: ButtonsPictures -> Picture
buttonsAsPicture bp = pictures [ greenButton bp
                                , redButton bp
                                , blueButton bp
                                , yellowButton bp ]
