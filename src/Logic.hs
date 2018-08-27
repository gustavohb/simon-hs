module Logic where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Game

update :: Float -> GameState -> GameState
update _ gs
  | timer gs' <= 0 = nextState gs'
  | otherwise      = gs'
    where
      gs' = gs {timer = (timer gs) - 1}

nextState :: GameState -> GameState
nextState gs = case status gs of
                    Playing   -> playSequence gs
                    Receiving -> receiveSequence gs
                    _         -> gs

playSequence :: GameState -> GameState
playSequence gs | isColorOn          = setColorOn Nothing colorDisplayTime gs
                | continuePlaying    = incPlayerPos $ setColorOn cc colorDisplayTime gs
                | otherwise          = changeToReceiving $ resetPlayerPos $ setColorOn Nothing timeout gs
  where
    isColorOn = colorOn gs /= Nothing
    cc = Just $ getCurrentColor gs
    continuePlaying = playerPos gs < seqPos gs

receiveSequence :: GameState -> GameState
receiveSequence gs | isValidColor gs = if continueSeq then
                                         incPlayerPos $ setColorOn Nothing timeout gs
                                       else
                                         incSeqPos $ changeToPlaying $ resetPlayerPos $ setColorOn Nothing colorDisplayTime gs
                   | otherwise       = changeToGameOver gs
  where
   nextPos = playerPos gs + 1
   continueSeq = nextPos < seqPos gs

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (MouseButton LeftButton) Up _ mousePos) gs = case status gs of
                                                                    Receiving -> gs'
                                                                    GameOver  -> initialState {colorSeq = genColorSeq gen maxSeqLen}
                                                                    _         -> gs
  where
    c   = mousePosAsButtonColor mousePos
    gs' = setColorOn (Just c) colorSelectTime gs
    gen = mkStdGen $ seqPos gs
handleInput _                                                 gs = gs

mousePosAsButtonColor :: (Float, Float) -> ButtonColor
mousePosAsButtonColor (x, y) | x < 0 && y < 0 = Yellow
                             | x < 0 && y > 0 = Green
                             | x > 0 && y > 0 = Red
                             | x > 0 && y < 0 = Blue

setColorOn :: Maybe ButtonColor -> Int -> GameState -> GameState
setColorOn c t gs = gs {colorOn = c, timer = t}

resetPlayerPos :: GameState -> GameState
resetPlayerPos gs = gs {playerPos = 0}

incPlayerPos :: GameState -> GameState
incPlayerPos gs = gs {playerPos = nextPos}
  where
    nextPos = playerPos gs + 1

incSeqPos :: GameState -> GameState
incSeqPos gs | nextPos > maxSeqLen = changeToFinished gs
             | otherwise           = gs {seqPos = nextPos, status = Playing}
  where nextPos = seqPos gs + 1

changeToReceiving :: GameState -> GameState
changeToReceiving gs = gs {status = Receiving}

changeToPlaying :: GameState -> GameState
changeToPlaying gs = gs {status = Playing}

changeToGameOver :: GameState -> GameState
changeToGameOver gs = gs {status = GameOver, colorOn = Nothing}

changeToFinished :: GameState -> GameState
changeToFinished gs = gs {status = Finished, colorOn = Nothing}

isValidColor :: GameState -> Bool
isValidColor gs = colorOn gs == cc
  where cc = Just $ getCurrentColor gs

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
