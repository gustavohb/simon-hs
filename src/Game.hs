module Game where

screenWidth :: Int
screenWidth = 480

screenHeight :: Int
screenHeight = 480

screenOffset :: Int
screenOffset = 100

fps :: Int
fps = 60

colorDisplayTime :: Int
colorDisplayTime = 30

initialDelay :: Int
initialDelay = 100

timeout :: Int
timeout = 260

maxSeqLen :: Int
maxSeqLen = 100

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
  , colorSeq = []
  , seqPos = 6
  , playerPos = 0
  }
