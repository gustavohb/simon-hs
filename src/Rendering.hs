module Rendering where

import Graphics.Gloss

import Game

backgroundColor = makeColorI 240 240 240 255

yellowOn = color $ makeColorI 255 221 97 255
yellowOff = color $ makeColorI 244 194 13 255

blueOn = color $ makeColorI 122 171 255 255
blueOff = color $ makeColorI 72 133 237 255

redOn = color $ makeColorI 255 87 89 255
redOff = color $ makeColorI 219 50 54 255

greenOn = color $ makeColorI 93 219 116 255
greenOff = color $ makeColorI 60 186 84 255

borderColor = color $ makeColorI 66 66 66 255

cCircleColor = color $ makeColorI 241 233 192 255

radius = 140.0
thickness = 100.0

textColor = color $ makeColorI 66 66 66 255

hBar = borderColor $ rectangleSolid 404 13
vBar = borderColor $ rectangleSolid 13 404

cCircle = color backgroundColor $ circleSolid 78
borderCircle = borderColor $ circleSolid 202

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
