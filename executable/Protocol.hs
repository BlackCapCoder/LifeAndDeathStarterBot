{-# LANGUAGE LambdaCase #-}
module Protocol where

import Control.Monad.State


type ID = Int

data Settings = Settings
  { timebank     :: Int
  , timePerMove  :: Int
  , playerNames  :: [String]
  , myName       :: String
  , myID         :: ID
  , maxRounds    :: Int
  , currentRound :: Int
  , game         :: Game
  }

data Game = Game
  { field     :: [Cell]
  , width     :: Int
  , height    :: Int
  , myCellCnt :: Int
  , opCellCnt :: Int
  }

data Cell   = Dead | Alive ID
data Vec    = Vec Int Int
data Action = Kill Vec | Birth Vec Vec Vec | Pass

type GameState a = StateT Settings IO a


instance Show Vec where
  show (Vec x y) = show x ++ ',' : show y

instance Show Action where
  show Pass          = "pass"
  show (Kill x)      = "kill " ++ show x
  show (Birth a b c) = unwords $ "birth" : map show [a, b, c]


defSettings :: Settings
defSettings = Settings
  { timebank     = 10000
  , timePerMove  = 500
  , playerNames  = mempty
  , myName       = mempty
  , myID         = 0
  , maxRounds    = 100
  , currentRound = 0
  , game         = defGame
  }

defGame :: Game
defGame = Game
  { field     = []
  , width     = 18
  , height    = 16
  , myCellCnt = 0
  , opCellCnt = 0
  }

runGameState :: GameState a -> Settings -> IO (a, Settings)
runGameState = runStateT

runGameState' :: GameState a -> IO (a, Settings)
runGameState' = flip runGameState defSettings


parseUpdate :: [String] -> GameState ()
parseUpdate = \case
  ("settings":"timebank":[t])      -> modify $ \s -> s { timebank = read t }
  ("settings":"time_per_move":[t]) -> modify $ \s -> s { timePerMove = read t }
  ("settings":"player_names":[t])  -> modify $ \s -> s { playerNames = split ',' t }
  ("settings":"your_bot":[t])      -> modify $ \s -> s { myName = t }
  ("settings":"field_width":[t])   -> modify $ \s -> s { game = (game s) { width = read t } }
  ("settings":"field_height":[t])  -> modify $ \s -> s { game = (game s) { height = read t } }
  ("settings":"max_rounds":[t])    -> modify $ \s -> s { maxRounds = read t }
  ("update":"game":"round":[t])    -> modify $ \s -> s { currentRound = read t }
  ("update":"game":"field":[t])    -> modify $ \s -> s { game = (game s) { field = parseCell <$> split ',' t } }
  ("update":p:"living_cells":[t])  -> do
    m <- gets myName
    if m == p then modify $ \s -> s { game = (game s) { myCellCnt = read t } }
             else modify $ \s -> s { game = (game s) { opCellCnt = read t } }
  _ -> return ()

parseCell :: String -> Cell
parseCell = \case
  "." -> Dead
  x   -> Alive $ read x


split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s  = x : split d (drop 1 y) where (x,y) = span (/= d) s
