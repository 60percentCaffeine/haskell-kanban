module TaskBot
  ( run
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- TODO: struct
data CardPriority = Highest | High | Medium | Low | Lowest
data Card = Card {
  title :: String,
  worker :: Int,
  priority :: CardPriority
}

data AppState = AppState {
  cards :: [[Card]],
  editing :: Maybe Card
}

users :: [String]
users = ["Undefined", "Bob", "Alice", "John", "Valera"]

rectangle :: Float -> Float -> Picture
rectangle w h
 = let  posX    = 0
        posY    = 0 -- -h/2
        x1      = posX
        x2      = posX + w
        y1      = posY
        y2      = posY + h
   in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

card :: Card -> Picture
card crd = Pictures [(Color white $ rectangle 300 100), (translate 10 60 $ Color black $ Scale 0.2 0.2 $ Text (title $ crd))]

cardList :: Float -> [Card] -> [Picture]
cardList _ [] = []
cardList offY (a: az) = ((translate 10 offY $ card a): (cardList (offY - 110) az))

column :: Float -> [Card] -> Picture
column h cards = Pictures ((rectangle 320 h) : (cardList (h - 110) cards))

columnList :: Float -> [[Card]] -> [Picture]
columnList _ [] = []
columnList offX (cards: cards_z) = ((translate offX 0 $ column 680 cards) : (columnList (offX + 330) cards_z))

drawApp :: AppState -> Picture
drawApp (AppState x _) = translate (-590) (-340) $ Pictures $ columnList 0 x

-- Handle events.
handleEvent :: Event -> AppState -> AppState
handleEvent _ state = state

-- simulation step
updateApp :: Float -> AppState -> AppState
updateApp _ x = x

run :: IO ()
run = play window background 1 defaultState drawApp handleEvent updateApp
  -- $ translate (-600) 350 $ scale 700 700 drawing
    where
      window = InWindow "Kanban Board" (1200, 700) (0, 0) 
      background = white 
      --drawing = Polygon [(0,0), (0,160), (80,160), (80,0)]
      defaultState = AppState [[(Card "Test Card 1" 0 Medium), (Card "Test Card 2" 0 Medium)], [(Card "Test Card 3" 0 Medium), (Card "Test Card 4" 0 Medium)], [(Card "Test Card 5" 0 Medium)]] Nothing