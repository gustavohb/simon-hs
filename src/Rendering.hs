module Rendering where

import Graphics.Gloss

import Game

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

drawScore :: GameState -> Picture
drawScore gs = case status gs of
                 GameOver -> scale 0.3 0.3 $ translate (-310) (-35) $ color white $ text $ "Try Again"
                 Finished -> scale 0.3 0.3 $ translate (-310) (-35) $ color white $ text $ "You Won!"
                 _        -> scale 0.4 0.4 $ translate (-35) (-45) $ color white $ text $ show (seqPos gs - 1)

renderState :: GameState -> Picture
renderState gs = pictures [ gameAsPicture gs
                          , drawScore gs
                          ]
