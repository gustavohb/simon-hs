module Logic where

import Graphics.Gloss.Data.ViewPort
import System.Random

import Game

update :: ViewPort -> Float -> GameState -> GameState
update _ _ gs
  | timer gs' <= 0 = playSequence gs'
  | otherwise      = gs'
    where
      gs' = gs {timer = (timer gs) - 1}

playSequence :: GameState -> GameState
playSequence gs | isColorOn          = setColorOn Nothing colorDisplayTime gs
                | continuePlaying    = incPlayerPos $ setColorOn cc colorDisplayTime gs
                | otherwise          = resetPlayerPos $ setColorOn Nothing initialDelay gs
  where
    isColorOn = colorOn gs /= Nothing
    cc = Just $ getCurrentColor gs
    continuePlaying = playerPos gs < seqPos gs

setColorOn :: Maybe ButtonColor -> Int -> GameState -> GameState
setColorOn c t gs = gs {colorOn = c, timer = t}

resetPlayerPos :: GameState -> GameState
resetPlayerPos gs = gs {playerPos = 0}

incPlayerPos :: GameState -> GameState
incPlayerPos gs = gs {playerPos = nextPos}
  where
    nextPos = playerPos gs + 1

getCurrentColor :: GameState -> ButtonColor
getCurrentColor gs = c
  where
    p = playerPos gs
    c = colorSeq gs !! p

genColorSeq :: StdGen -> Int -> [ButtonColor]
genColorSeq gen len = map intToColor (take len $ randomRs (1,4) gen)

intToColor :: Int -> ButtonColor
intToColor n | n == 1 = Yellow
             | n == 2 = Green
             | n == 3 = Red
             | n == 4 = Blue
