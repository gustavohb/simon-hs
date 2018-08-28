module Rendering where

import Graphics.Gloss

import Game

backgroundColor = makeColorI 222 222 222 255

borderColor = color $ makeColorI 53 52 63 255

yellowOn = color $ makeColorI 255 247 148 255
yellowOff = color $ makeColorI 235 222 68 255

blueOn = color $ makeColorI 109 200 253 255
blueOff = color $ makeColorI 35 161 232 255

redOn = color $ makeColorI 255 107 97 255
redOff = color $ makeColorI 250 49 36 255

greenOn = color $ makeColorI 102 251 179 255
greenOff = color $ makeColorI 31 202 119 255

textColor = color $ black

radius = 140.0
thickness = 100.0

barLength = 404
barThickness = 13

greenArc  = thickArc 90.0 180.0 radius thickness
redArc    = thickArc 0.0  90.0 radius thickness
yellowArc = thickArc 180.0 270.0 radius thickness
blueArc   = thickArc 270.0 359.9 radius thickness

hBar = borderColor $ rectangleSolid barLength barThickness
vBar = borderColor $ rectangleSolid barThickness barLength



cCircle = color backgroundColor $ circleSolid 78

borderCircle = borderColor $ circleSolid 202

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
buttonsAsPicture bp = pictures [ borderCircle
                               , greenButton bp
                               , redButton bp
                               , blueButton bp
                               , yellowButton bp
                               , hBar
                               , vBar
                               , cCircle ]

drawScore :: GameState -> Picture
drawScore gs = case status gs of
                 GameOver -> scale 0.2 0.2 $ translate (-310) (-35) $ textColor $ text $ "Try Again"
                 Finished -> scale 0.2 0.2 $ translate (-310) (-35) $ textColor $ text $ "You Won!"
                 _        -> scale 0.3 0.3 $ translate (-35) (-45) $ textColor $ text $ show (seqPos gs - 1)

renderState :: GameState -> Picture
renderState gs = pictures [ gameAsPicture gs
                          , drawScore gs
                          ]
