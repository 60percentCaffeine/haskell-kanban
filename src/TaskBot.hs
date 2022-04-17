module TaskBot
  ( run
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Consts

-- TODO: struct
data CardPriority = Highest | High | Medium | Low | Lowest
data Card = Card {
  title :: String,
  worker :: Int,
  priority :: CardPriority
}

instance Eq CardPriority where
  (==) Highest Highest = True
  (==) High High = True
  (==) Medium Medium = True
  (==) Low Low = True
  (==) Lowest Lowest = True
  (==) _ _ = False

instance Eq Card where
  Card x1 y1 z1 == Card x2 y2 z2 = (x1 == x2) && (y1 == y2) && (z1 == z2)

type Column = Maybe Int

data AppState = AppState {
  cards :: [[Card]],
  editing :: Maybe Card,
  moving :: Maybe Card,
  movingPos :: Maybe (Float, Float),
  _mouse :: (Float, Float),
  _column :: Column,
  _card :: Maybe Card
}

delete :: Eq a => a -> [a] -> [a]
delete el [] = []
delete el (x: xz) | (x == el) = xz
  | otherwise = (x: (delete el xz))

-- it might be better to have a dedicated User type
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

debugColumn :: Column -> String
debugColumn Nothing = "Nothing"
debugColumn (Just x) = show x

debugCard :: Maybe Card -> String
debugCard Nothing = "Nothing"
debugCard (Just x) = title x

debugInfo :: (Float, Float) -> Column -> Maybe Card -> Maybe Card -> Picture
debugInfo (x,y) col card moving = Text ("mouse coords: (" ++ (show x) ++ "," ++ (show y) ++ "), column:" ++ (debugColumn col) ++ ", card:" ++ (debugCard card) ++ ", moving:" ++ (debugCard moving))

-- if the currently edited card is Nothing, draw the board
-- else draw the card editing interface
drawApp :: AppState -> Picture
drawApp (AppState x _ cardBeingMoved movedCoords mouseCoords mouseColumn mouseCard) = translate (-590) (-340) $ Pictures $ (columnList 0 x) ++ [(translate (0) (0) $ Color red $ Scale 0.2 0.2 $ debugInfo mouseCoords mouseColumn mouseCard cardBeingMoved)]

-- first argument is the column number/counter
findColumnByCoords :: Int -> Float -> Float -> Column
findColumnByCoords 3 _ _ = Nothing
findColumnByCoords col x y
  | ((y > 340) || (y < -340)) = Nothing
  | ((x > -590) && (x < -270)) = Just col
  | otherwise = findColumnByCoords (col + 1) (x - 330) y

findCardByCoords :: Column -> Maybe [Card] -> Float -> Float -> Maybe Card
findCardByCoords Nothing _ _ _ = Nothing
findCardByCoords _ Nothing _ _ = Nothing
findCardByCoords (Just coln) (Just []) _ _ = Nothing
findCardByCoords (Just coln) (Just (a: az)) x y
  | ((y2 < 330) && (y2 > 230)) = (Just a)
  | otherwise = findCardByCoords (Just coln) (Just az) x (y + 110)
  where
    x2 = x - (fromIntegral $ 330*coln)
    y2 = y

indexColumn :: Column -> [[Card]] -> Maybe [Card]
indexColumn Nothing _ = Nothing
indexColumn (Just x) y = Just (y !! x)

removeCard :: Int -> [[Card]] -> Card -> [[Card]]
removeCard _ [] _ = []
removeCard 0 (x: xz) card = ((delete card x): xz)
removeCard n (x: xz) card = (x: (removeCard (n-1) xz card))

takeCardAt :: [[Card]] -> Float -> Float -> Maybe ([[Card]], Card)
takeCardAt cards x y
    | (Just justCard) <- card
    , (Just justColn) <- coln
    = Just $ ((removeCard justColn cards justCard), justCard)

    | otherwise = Nothing

    where
      coln = findColumnByCoords 0 x y
      card = findCardByCoords coln (indexColumn coln cards) x y

startMovingCard :: AppState -> Float -> Float -> Maybe AppState
startMovingCard (AppState cards Nothing Nothing Nothing a b c) x y
    | Just (newCards, card) <- takeCardAt cards x y
      = Just $ AppState newCards Nothing (Just card) (Just (x,y)) a b c

    | otherwise = Nothing

-- Handle events.
-- Handle mouse clicks on cards (start editing the card)
handleEvent :: Event -> AppState -> AppState
handleEvent event state
{-
    -- If the mouse has moved, move the card as well
    | EventMotion (x, y)    <- event
    , AppState cards Nothing (Just moving)    <- state
    = AppState cards Nothing $ Just $ translate x y $ moving
-}
    -- Start moving a card on left clicking a card
    | EventKey (MouseButton LeftButton) Down _ (x, y) <- event
    , AppState cards Nothing Nothing Nothing _ _ _ <- state
    , Just newState <- (startMovingCard state x y) -- this feels illegal
    = newState

    | EventKey (MouseButton LeftButton) Up _ (x, y) <- event
    , AppState cards Nothing (Just moving) (Just (x2,y2)) a b c <- state
    = AppState cards Nothing Nothing Nothing a b c

    | otherwise = state
{-
handleEvent (EventMotion (x,y)) (AppState cards Nothing Nothing _ _ _) = AppState cards Nothing Nothing (x,y) coln (findCardByCoords coln col x y)
  where
    coln = findColumnByCoords 0 x y
    col = indexColumn coln cards
-}
--handleEvent _ state = state

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
      defaultState = AppState [[(Card "Test Card 1" 0 Medium), (Card "Test Card 2" 0 Medium)], [(Card "Test Card 3" 0 Medium), (Card "Test Card 4" 0 Medium)], [(Card "Test Card 5" 0 Medium)]] Nothing Nothing Nothing (0,0) Nothing Nothing