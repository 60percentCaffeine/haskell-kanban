module Funcs where

import Graphics.Gloss
import Types

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete el (x: xz) | (x == el) = xz
  | otherwise = (x: (delete el xz))

deleteAt :: [a] -> Int -> [a]
deleteAt [] _ = []
deleteAt (_: az) 0 = az
deleteAt (a: az) n = (a: deleteAt az (n-1))

replaceAt :: [a] -> Int -> a -> [a]
replaceAt [] _ _ = []
replaceAt (_: az) 0 el = (el: az)
replaceAt (a: az) n el = (a: replaceAt az (n-1) el)

pop :: [a] -> [a]
pop [] = []
pop [_] = []
pop (x: xz) = (x: (pop xz))

rectangle :: Float -> Float -> Picture
rectangle w h
 = let  posX    = 0
        posY    = 0 -- -h/2
        x1      = posX
        x2      = posX + w
        y1      = posY
        y2      = posY + h
   in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

drawCard :: Card -> Picture
drawCard crd = Pictures [
  (Color white $ rectangle 300 100),
  (translate 10 60 $ Color black $ Scale 0.2 0.2 $ Text (cardTitle $ crd))]

cardList :: Float -> [Card] -> [Picture]
cardList _ [] = []
cardList offY (a: az) = ((translate 10 offY $ drawCard a): (cardList (offY - 110) az))

column :: Float -> [Card] -> Picture
column h cards = Pictures ((rectangle 320 h) : (cardList (h - 110) cards))

columnList :: Float -> [[Card]] -> [Picture]
columnList _ [] = []
columnList offX (cards: cards_z) =
  ((translate offX 0 $ column 680 cards) : (columnList (offX + 330) cards_z))

-- first argument is the column number/counter
findColumnByCoords :: Int -> Float -> Float -> Column
findColumnByCoords 3 _ _ = Nothing
findColumnByCoords col x y
  | ((y > 340) || (y < -340)) = Nothing
  | ((x > -590) && (x < -270)) = Just col
  | otherwise = findColumnByCoords (col + 1) (x - 330) y

findCardIndexByCoords :: Column -> Maybe [Card] -> Float -> Float -> Int -> Maybe Int
findCardIndexByCoords Nothing _ _ _ _ = Nothing
findCardIndexByCoords _ Nothing _ _ _ = Nothing
findCardIndexByCoords (Just _) (Just []) _ _ _ = Nothing
findCardIndexByCoords (Just coln) (Just (_: az)) x y n
  | ((y2 < 330) && (y2 > 230)) = (Just n)
  | otherwise = findCardIndexByCoords (Just coln) (Just az) x (y + 110) (n + 1)
  where
--    x2 = x - (fromIntegral $ 330*coln)
    y2 = y

findCardByCoords :: Column -> Maybe [Card] -> Float -> Float -> Maybe Card
findCardByCoords coln colm x y
  | (Just i) <- idx
  , (Just col) <- colm
  = Just (col !! i)

  | otherwise = Nothing
  
  where
    idx = (findCardIndexByCoords coln colm x y 0)

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

insertAtPos :: [Card] -> Maybe Int -> Card -> [Card]
insertAtPos cards Nothing crd = cards ++ [crd]
insertAtPos cards (Just 0) crd = (crd: cards)
insertAtPos (x: xz) (Just n) crd = (x: (insertAtPos xz (Just (n - 1)) crd))
insertAtPos [] _ crd = [crd]

addCardTo :: [[Card]] -> Int -> Maybe Int -> Card -> [[Card]]
addCardTo (a: az) 0 cardToReplace card = (newa: az)
  where newa = insertAtPos a cardToReplace card
addCardTo (a: az) coln cardToReplace card = (a: (addCardTo az (coln - 1) cardToReplace card))
addCardTo [] _ _ crd = [[crd]]