module Game where

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
