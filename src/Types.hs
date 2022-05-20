module Types where

-- TODO: struct
--data CardPriority = Highest | High | Medium | Low | Lowest
data Card = Card {
  cardId :: Maybe Int,
  cardTitle :: String,
  cardWorker :: Int,
  cardTags :: [String]
}

{-
instance Eq CardPriority where
  (==) Highest Highest = True
  (==) High High = True
  (==) Medium Medium = True
  (==) Low Low = True
  (==) Lowest Lowest = True
  (==) _ _ = False
-}

instance Eq Card where
  Card x1 y1 z1 _ == Card x2 y2 z2 _ = (x1 == x2) && (y1 == y2) && (z1 == z2)

type Column = Maybe Int

data MovingCard = MovingCard {
  movingCard :: Card,
  movingPos :: (Float, Float),
  movingPrevCol :: Int,
  movingPrevPos :: Int
}

type TextScreenFunc = (AppState -> String -> AppState)
type SelectScreenFunc = (AppState -> [String] -> Int -> AppState)

data AppScreen =
  None
  | EditScreen MovingCard
  | MoveScreen MovingCard
  | TextScreen String (String -> AppState)
  | SelectScreen [String] Int ([String] -> Int -> AppState)
  | ListUpdateScreen [String] Int (AppState -> [String] -> AppState)

listUpdateScreenGetList :: AppScreen -> [String]
listUpdateScreenGetList (ListUpdateScreen l _ _) = l
listUpdateScreenGetList _ = []

data AppState = AppState {
  stateCards :: [[Card]],
  stateScreen :: AppScreen,
  stateUsers :: [String],
  _mouse :: (Float, Float),
  _column :: Column,
  _card :: Maybe Card
}

editing :: AppState -> Maybe MovingCard
editing state
  | (EditScreen mcard) <- (stateScreen state) = Just mcard
  | otherwise = Nothing

moving :: AppState -> Maybe MovingCard
moving state
  | (MoveScreen mcard) <- (stateScreen state) = Just mcard
  | otherwise = Nothing