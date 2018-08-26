module Game where

colorDisplayTime :: Int
colorDisplayTime = 30

initialDelay :: Int
initialDelay = 100

data GameState = GameState
  { colorOn :: Maybe ButtonColor
  , timer :: Int
  , status :: GameStatus
  , colorSeq :: [ButtonColor]
  , seqPos :: Int
  , playerPos   :: Int } deriving Show

data ButtonColor = Green
                 | Red
                 | Blue
                 | Yellow
  deriving (Show, Eq)

data GameStatus = Playing
                | Receiving
                | Finished
                | GameOver
  deriving (Show, Eq)

initialState :: GameState
initialState = GameState
  { colorOn = Nothing
  , timer = initialDelay
  , status = Playing
  , colorSeq = [Green, Red, Blue, Yellow, Red, Yellow, Blue]
  , seqPos = 6
  , playerPos = 0
  }
