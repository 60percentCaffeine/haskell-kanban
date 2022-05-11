module KanbanMain
  ( run
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Consts
import Types
import Funcs
--import DatabaseModule

debugColumn :: Column -> String
debugColumn Nothing = "Nothing"
debugColumn (Just x) = show x

debugCard :: Maybe Card -> String
debugCard Nothing = "Nothing"
debugCard (Just x) = cardTitle x

debugMovingCard :: Maybe MovingCard -> String
debugMovingCard Nothing = "Nothing"
debugMovingCard (Just x) = cardTitle $ movingCard x

debugInfo :: (Float, Float) -> Column -> Maybe Card -> Maybe MovingCard -> Picture
debugInfo (x,y) col currentCard currentMoving = Text ("mouse coords: (" ++ (show x) ++ "," ++ (show y) ++
  "), column:" ++ (debugColumn col) ++
  ", card:" ++ (debugCard currentCard) ++
  ", moving:" ++ (debugMovingCard currentMoving))

data EditField = NameField | WorkerField | TagsField

debugField :: Maybe EditField -> String
debugField Nothing = "Nothing"
debugField (Just NameField) = "NameField"
debugField (Just WorkerField) = "WorkerField"
debugField (Just TagsField) = "TagsField"

detectEditField :: Float -> Float -> Maybe EditField
detectEditField x y
  | ((x > editCardNameFieldXStart) && (y < editCardNameFieldYEnd) && (y > editCardNameFieldYStart)) = Just NameField
  | ((x > editCardWorkerFieldXStart) && (y < editCardWorkerFieldYEnd) && (y > editCardWorkerFieldYStart)) = Just WorkerField
  | ((x > editCardTagsFieldXStart) && (y < editCardTagsFieldYEnd) && (y > editCardTagsFieldYStart)) = Just TagsField
  | otherwise = Nothing

updateNameField :: AppState -> String -> AppState
updateNameField state str
  | EditScreen mcard <- stateScreen state = let
      card = movingCard mcard
      card2 = card {cardTitle = str}
      mcard2 = mcard {movingCard = card2}
    in state {stateScreen = (EditScreen mcard2)}
  | otherwise = state -- something went horribly wrong

updateWorkerField :: AppState -> [String] -> Int -> AppState
updateWorkerField state _ idx
  | EditScreen mcard <- stateScreen state = let
      card = movingCard mcard
      card2 = card {cardWorker = idx}
      mcard2 = mcard {movingCard = card2}
    in state {stateScreen = (EditScreen mcard2)}

  | otherwise = state

updateTagList :: AppScreen -> AppState -> [String] -> AppState
updateTagList screen state tags
  | EditScreen mcard <- screen = let
      card = movingCard mcard
      card2 = card {cardTags = tags}
      mcard2 = mcard {movingCard = card2}
    in state {stateScreen = (EditScreen mcard2)}

  | otherwise = state

handleEditClick :: AppState -> Float -> Float -> AppState
handleEditClick state x y
  | Just NameField <- field
  , Just mcard <- editing state
  = startTextScreen state (cardTitle $ movingCard mcard) updateNameField

  | Just WorkerField <- field
  , Just mcard <- editing state
  , card <- movingCard mcard
  = startSelectScreen state (stateUsers state) (cardWorker card) updateWorkerField

  | Just TagsField <- field
  , Just mcard <- editing state
  , card <- movingCard mcard
  = state {stateScreen = ListUpdateScreen (cardTags card) 0 (updateTagList $ stateScreen state)}

  | Nothing <- field = state

  where field = detectEditField x y
handleEditClick state _ _ = state

showCardTags :: [String] -> String
showCardTags [] = []
showCardTags (a: az) = a ++ " " ++ (showCardTags az)

showListValue :: [String] -> Int -> String
showListValue list n
  | (length list == 0) = ""
  | otherwise = list !! n

-- if the currently edited card is Nothing, draw the board
-- else draw the card editing interface
drawApp :: AppState -> Picture
drawApp state
  | (Just edi) <- editing state
  = translate (leftSide + 20) (300)
  $ Pictures [
    -- draw name
    Scale textScale textScale $ Text editCardNameTip,
    translate 100 0 $ Scale textScale textScale $ Text $ cardTitle $ movingCard edi,

    -- draw worker
    translate 0 (-50) $ Scale textScale textScale $ Text editCardPersonTip,
    translate 100 (-50) $ Scale textScale textScale $ Text ((stateUsers state) !! (cardWorker $ movingCard edi)),

    -- draw tags
    translate 0 (-100) $ Scale textScale textScale $ Text "Tags:",
    translate 100 (-100) $ Scale textScale textScale $ Text $ showCardTags $ cardTags $ movingCard edi,

    -- save/remove card message
    translate 0 (-150) $ Scale textScale textScale $ Text editCardExitTip,
    translate 0 (-200) $ Scale textScale textScale $ Text editCardRemoveTip,

    -- draw debug info
    translate (-20) (-height + 60) $ Scale textScale textScale $ Color red
      $ Text ("Mouse:(" ++ (show x) ++ "," ++ (show y) ++
        "), edit field:" ++ (debugField $ detectEditField x y))
  ]
--  = Scale textScale textScale $ Text "Editing a card, Q to save & exit"

  | TextScreen str _ <- stateScreen state
  = translate (leftSide + 20) (300)
  $ Pictures [
    Scale textScale textScale $ Text textInputTip,
    translate 400 0 $ Scale textScale textScale $ Text str,
    translate 0 (-50) $ Scale textScale textScale $ Text textInputControlsTip
  ]

  | SelectScreen list idx _ <- stateScreen state
  = translate (leftSide + 20) (300)
  $ Pictures [
    Scale textScale textScale $ Text selectScreenTip,
    translate 400 0 $ Scale textScale textScale $ Text (list !! idx),
    translate 0 (-50) $ Scale textScale textScale $ Text selectScreenControlsTip
  ]

  | ListUpdateScreen list idx _ <- stateScreen state
  = translate (leftSide + 20) (300)
  $ Pictures [
    Scale textScale textScale $ Text listUpdateScreenTip,
    translate 400 0 $ Scale textScale textScale $ Text (showListValue list idx),
    translate 0 (-50) $ Scale textScale textScale $ Text listUpdateScreenPickTip,
    translate 0 (-100) $ Scale textScale textScale $ Text listUpdateScreenAddTip,
    translate 0 (-150) $ Scale textScale textScale $ Text listUpdateScreenQuitTip
  ]
  
  | otherwise
  = translate (leftSide + 10) (-340)
  $ Pictures
  $ (columnList 0 (stateCards state))
    ++ [(translate (0) (0)
      $ Color red
      $ Scale textScale textScale
      $ debugInfo (_mouse state) (_column state) (_card state) (moving state))]

  where
    x = fst $ _mouse state
    y = snd $ _mouse state

startMovingCard :: AppState -> Float -> Float -> Maybe AppState
startMovingCard state x y
    | Just (newCards, card) <- takeCardAt (stateCards state) x y
    , Just (coln) <- (_column state)
    , Just cardn <- findCardIndexByCoords (Just coln) (Just $ (stateCards state) !! coln) x y 0
    , ((isNothing $ editing state) && (isNothing $ moving state))
      = Just $ state {stateCards = newCards, stateScreen = (MoveScreen $ MovingCard card (x,y) coln cardn)}

    | otherwise = Nothing

startEditingCard :: AppState -> Float -> Float -> Maybe AppState
startEditingCard state x y
    | Just (newCards, card) <- takeCardAt (stateCards state) x y
    , Just coln <- (_column state)
    , Just cardn <- findCardIndexByCoords (Just coln) (Just $ (stateCards state) !! coln) x y 0
    , ((isNothing $ editing state) && (isNothing $ editing state))
      = Just $ state {stateCards = newCards, stateScreen = (EditScreen $ MovingCard card (x,y) coln cardn)}

    | otherwise = Nothing

startCreatingCard :: AppState -> Int -> Float -> Float -> AppState
startCreatingCard state coln x y = state {stateScreen =
  (EditScreen $ MovingCard newcard (x,y) coln (length $ (stateCards state !! coln)))}

  where newcard = Card "New Card" 0 []

-- starts a text screen
-- the textscreenfunc modifies the state BEFORE the text screen started
-- so it still has the previous screen
startTextScreen :: AppState -> String -> TextScreenFunc -> AppState
startTextScreen state str f = state {stateScreen = TextScreen str (f state)}

startSelectScreen :: AppState -> [String] -> Int -> SelectScreenFunc -> AppState
startSelectScreen state list idx f = state {stateScreen = SelectScreen list idx (f state)}

updateUserList :: AppScreen -> AppState -> [String] -> AppState
updateUserList screen state users = state {stateScreen = screen, stateUsers = ("Undefined": users)}

updateListElement :: AppState -> String -> AppState
updateListElement state name
  | ListUpdateScreen list idx f <- stateScreen state
    = state {stateScreen = ListUpdateScreen (replaceAt list idx name) idx f}

  | otherwise = state

removeWorkerFromColumn :: [Card] -> Int -> [Card]
removeWorkerFromColumn [] _ = []
removeWorkerFromColumn (a: az) n
  | (cardWorker a == n) = (a {cardWorker = 0}: removeWorkerFromColumn az n)
  | (cardWorker a > n) = (a {cardWorker = ((cardWorker a) - 1)}: removeWorkerFromColumn az n)
  | otherwise = (a: removeWorkerFromColumn az n)

removeWorker :: [[Card]] -> Int -> [[Card]]
removeWorker [] _ = []
removeWorker (a: az) n = (removeWorkerFromColumn a n: removeWorker az n)

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
    --
    -- Default view
    --

    -- Add a new card on clicking an empty space in the column
    | EventKey (MouseButton LeftButton) Down _ (x, y) <- event
    , Just coln <- (findColumnByCoords 0 x y)
    , Nothing <- (findCardIndexByCoords (Just coln) (Just $ (stateCards state) !! coln) x y 0)
    , None <- stateScreen state
    = startCreatingCard state coln x y

    -- Start moving a card on left clicking it
    | EventKey (MouseButton LeftButton) Down _ (x, y) <- event
    , Just newState <- (startMovingCard state x y) -- this feels illegal
    , ((isNothing $ editing state) && (isNothing $ moving state))
    = newState

    -- Stop moving the card
    | EventKey (MouseButton LeftButton) Up _ (x, y) <- event
    , AppState cards (MoveScreen mov) _ _ _ _ <- state
    , Just coln <- (findColumnByCoords 0 x y)
    , cardn <- (findCardIndexByCoords (Just coln) (Just $ cards !! coln) x y 0)
    = state {stateCards = (addCardTo cards coln cardn (movingCard mov)), stateScreen = None}

    -- Start editing a card on right clicking it
    | EventKey (MouseButton RightButton) Down _ (x, y) <- event
    , Just newState <- (startEditingCard state x y)
    , ((isNothing $ editing state) && (isNothing $ moving state))
    = newState

    -- Start editing the personnel list on M
    | EventKey (Char 'm') Down _ _ <- event
    , None <- (stateScreen state)
    = state {stateScreen = ListUpdateScreen (tail $ stateUsers state) 0 (updateUserList None)}

    --
    -- Editing view
    --

    -- Stop editing on Q
    | EventKey (Char 'q') Down _ _ <- event
    , AppState cards (EditScreen edi) _ _ _ _ <- state
    = state {
      stateCards = (addCardTo cards (movingPrevCol edi) (Just $ movingPrevPos edi) (movingCard edi)), 
      stateScreen = None
    }

    -- Remove card on D
    | EventKey (Char 'd') Down _ _ <- event
    , AppState _ (EditScreen _) _ _ _ _ <- state
    = state {stateScreen = None}

    -- Handle mouse clicks in editing view
    | EventKey (MouseButton LeftButton) Up _ (x, y) <- event
    , EditScreen _ <- stateScreen state = handleEditClick state x y

    -- Update debug info in Kanban view
    | EventMotion (x, y) <- event
    , ((isNothing $ editing state) && (isNothing $ moving state))
    = let coln = findColumnByCoords 0 x y
          col = indexColumn coln (stateCards state)
      in state {_mouse = (x,y), _column = coln, _card = (findCardByCoords coln col x y)}

    -- Update debug info in Editing view
    | EventMotion (x, y) <- event
    , EditScreen _ <- (stateScreen state)
    = state {_mouse = (x, y)}

    --
    -- Text view
    --

    -- Handle key presses in Text view
    | EventKey (Char c) Down _ _ <- event
    , TextScreen str f <- stateScreen state
    = state {stateScreen = TextScreen (str ++ [c]) f}

    -- Remove latest symbol on pressing Delete in Text view
    | EventKey (SpecialKey KeyDelete) Down _ _ <- event
    , TextScreen str f <- stateScreen state
    = state {stateScreen = TextScreen (pop str) f}

    -- Submit the result on pressing right arrow in Text view
    | EventKey (SpecialKey KeyRight) Down _ _ <- event
    , TextScreen str f <- stateScreen state
    = f str

    --
    -- Select view
    --

    -- Submit the result on pressing down arrow in Text view
    | EventKey (SpecialKey KeyDown) Down _ _ <- event
    , SelectScreen list idx f <- stateScreen state
    = f list idx

    -- Cycle through choices on pressing right arrow
    | EventKey (SpecialKey KeyRight) Down _ _ <- event
    , SelectScreen list idx f <- stateScreen state
    , newidx <- idx + 1
    , ((length list) > newidx)
    = state {stateScreen = SelectScreen list newidx f}

    -- Cycle back through choices on pressing left arrow
    | EventKey (SpecialKey KeyLeft) Down _ _ <- event
    , SelectScreen list idx f <- stateScreen state
    , newidx <- idx - 1
    , (newidx >= 0)
    = state {stateScreen = SelectScreen list newidx f}

    --
    -- List update view
    --

    -- Cycle through choices on pressing right arrow
    | EventKey (SpecialKey KeyRight) Down _ _ <- event
    , ListUpdateScreen list idx f <- stateScreen state
    , newidx <- idx + 1
    , ((length list) > newidx)
    = state {stateScreen = ListUpdateScreen list newidx f}

    -- Cycle back through choices on pressing left arrow
    | EventKey (SpecialKey KeyLeft) Down _ _ <- event
    , ListUpdateScreen list idx f <- stateScreen state
    , newidx <- idx - 1
    , (newidx >= 0)
    = state {stateScreen = ListUpdateScreen list newidx f}

    -- Remove element from the list on D
    | EventKey (Char 'd') Down _ _ <- event
    , ListUpdateScreen list idx f <- stateScreen state
    , cards <- stateCards state
    = state {
      stateScreen = ListUpdateScreen (deleteAt list idx) 0 f,
      stateCards = removeWorker cards (idx + 1)
    }

    -- Edit element on E
    | EventKey (Char 'e') Down _ _ <- event
    , ListUpdateScreen list idx _ <- stateScreen state
    = startTextScreen state (list !! idx) updateListElement

    -- Add new element on A
    | EventKey (Char 'a') Down _ _ <- event
    , ListUpdateScreen list _ f <- stateScreen state
    = state {stateScreen = ListUpdateScreen (list ++ ["New Element"]) (length list) f}

    -- Quit&save on Q
    | EventKey (Char 'q') Down _ _ <- event
    , ListUpdateScreen list _ f <- stateScreen state
    = f state list

    | otherwise = state

-- simulation step
updateApp :: Float -> AppState -> AppState
updateApp _ x = x

run :: IO ()
run = play window background 1 defaultState drawApp handleEvent updateApp
  -- $ translate (-600) 350 $ scale 700 700 drawing
    where
      window = InWindow "Kanban Board" (round width, round height) (0, 0) 
      background = white 
      --drawing = Polygon [(0,0), (0,160), (80,160), (80,0)]
      defaultState = AppState {
        stateCards = [
          [(Card "Test Card 1" 0 []), (Card "Test Card 2" 0 [])],
          [(Card "Test Card 3" 0 []), (Card "Test Card 4" 0 [])],
          [(Card "Test Card 5" 0 [])]
        ]
        , stateScreen = None
        , stateUsers = defaultUserList
        , _mouse = (0,0)
        , _column = Nothing
        , _card = Nothing
      }