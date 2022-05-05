module TaskBot
  ( run
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Consts
import Types
import Funcs

-- it might be better to have a dedicated User type
users :: [String]
users = ["Undefined", "Bob", "Alice", "John", "Valera"]

debugColumn :: Column -> String
debugColumn Nothing = "Nothing"
debugColumn (Just x) = show x

debugCard :: Maybe Card -> String
debugCard Nothing = "Nothing"
debugCard (Just x) = title x

debugMovingCard :: Maybe MovingCard -> String
debugMovingCard Nothing = "Nothing"
debugMovingCard (Just x) = title $ movingCard x

debugInfo :: (Float, Float) -> Column -> Maybe Card -> Maybe MovingCard -> Picture
debugInfo (x,y) col card moving = Text ("mouse coords: (" ++ (show x) ++ "," ++ (show y) ++ "), column:" ++ (debugColumn col) ++ ", card:" ++ (debugCard card) ++ ", moving:" ++ (debugMovingCard moving))

data EditField = NameField | WorkerField

debugField :: Maybe EditField -> String
debugField Nothing = "Nothing"
debugField (Just NameField) = "NameField"
debugField (Just WorkerField) = "WorkerField"

detectEditField :: Float -> Float -> Maybe EditField
detectEditField x y
  | ((x > (-580)) && (y < 325) && (y > 290)) = Just NameField
  | ((x > (-580)) && (y < 280) && (y > 240)) = Just WorkerField
  | otherwise = Nothing

updateNameField :: AppState -> String -> AppState
updateNameField state str
  | EditScreen mcard <- screen state = let
      card = movingCard mcard
      card2 = card {title = str}
      mcard2 = mcard {movingCard = card2}
    in state {screen = (EditScreen mcard2)}
  | otherwise = state -- something went horribly wrong

updateWorkerField :: AppState -> [String] -> Int -> AppState
updateWorkerField state _ idx
  | EditScreen mcard <- screen state = let
      card = movingCard mcard
      card2 = card {worker = idx}
      mcard2 = mcard {movingCard = card2}
    in state {screen = (EditScreen mcard2)}

  | otherwise = state

handleEditClick :: AppState -> Float -> Float -> AppState
handleEditClick state x y
  | Just NameField <- field
  , Just mcard <- editing state
  = startTextScreen state (title $ movingCard mcard) updateNameField

  | Just WorkerField <- field
  , Just mcard <- editing state
  , card <- movingCard mcard
  = startSelectScreen state users (worker card) updateWorkerField

  | Nothing <- field = state

  where field = detectEditField x y

-- if the currently edited card is Nothing, draw the board
-- else draw the card editing interface
drawApp :: AppState -> Picture
drawApp state
  | (Just edi) <- editing state
  = translate (-580) (300)
  $ Pictures [
    -- draw name
    Scale 0.2 0.2 $ Text "Name:",
    translate 100 0 $ Scale 0.2 0.2 $ Text $ title $ movingCard edi,

    -- draw worker
    translate 0 (-50) $ Scale 0.2 0.2 $ Text "Person:",
    translate 100 (-50) $ Scale 0.2 0.2 $ Text (users !! (worker $ movingCard edi)),

    -- save/remove card message
    translate 0 (-100) $ Scale 0.2 0.2 $ Text "Press Q to save & exit",
    translate 0 (-150) $ Scale 0.2 0.2 $ Text "Press D to remove the card",

    -- draw debug info
    translate (-20) (-640) $ Scale 0.2 0.2 $ Color red $ Text ("Mouse:(" ++ (show x) ++ "," ++ (show y) ++ "), edit field:" ++ (debugField $ detectEditField x y))
  ]
--  = Scale 0.2 0.2 $ Text "Editing a card, Q to save & exit"

  | TextScreen str _ <- screen state
  = translate (-580) (300)
  $ Pictures [
    Scale 0.2 0.2 $ Text "Enter the new value:",
    translate 400 0 $ Scale 0.2 0.2 $ Text str,
    translate 0 (-50) $ Scale 0.2 0.2 $ Text "Delete to remove last letter, right arrow to submit"
  ]

  | SelectScreen list idx _ <- screen state
  = translate (-580) (300)
  $ Pictures [
    Scale 0.2 0.2 $ Text "Pick the new value:",
    translate 400 0 $ Scale 0.2 0.2 $ Text (list !! idx),
    translate 0 (-50) $ Scale 0.2 0.2 $ Text "Use left/right arrows to pick, down arrow to submit"
  ]
  
  | otherwise
  = translate (-590) (-340)
  $ Pictures
  $ (columnList 0 (cards state))
    ++ [(translate (0) (0)
      $ Color red
      $ Scale 0.2 0.2
      $ debugInfo (_mouse state) (_column state) (_card state) (moving state))]

  where
    x = fst $ _mouse state
    y = snd $ _mouse state

startMovingCard :: AppState -> Float -> Float -> Maybe AppState
startMovingCard state x y
    | Just (newCards, card) <- takeCardAt (cards state) x y
    , Just (coln) <- (_column state)
    , Just cardn <- findCardIndexByCoords (Just coln) (Just $ (cards state) !! coln) x y 0
    , ((isNothing $ editing state) && (isNothing $ moving state))
      = Just $ state {cards = newCards, screen = (MoveScreen $ MovingCard card (x,y) coln cardn)}

    | otherwise = Nothing

startEditingCard :: AppState -> Float -> Float -> Maybe AppState
startEditingCard state x y
    | Just (newCards, card) <- takeCardAt (cards state) x y
    , Just coln <- (_column state)
    , Just cardn <- findCardIndexByCoords (Just coln) (Just $ (cards state) !! coln) x y 0
    , ((isNothing $ editing state) && (isNothing $ editing state))
      = Just $ state {cards = newCards, screen = (EditScreen $ MovingCard card (x,y) coln cardn)}

    | otherwise = Nothing

startCreatingCard :: AppState -> Int -> Float -> Float -> AppState
startCreatingCard state coln x y = state {screen = (EditScreen $ MovingCard newcard (x,y) coln (length $ (cards state !! coln)))}
  where newcard = Card "New Card" 0 Medium

-- starts a text screen
-- the textscreenfunc modifies the state BEFORE the text screen started
-- so it still has the previous screen
startTextScreen :: AppState -> String -> TextScreenFunc -> AppState
startTextScreen state str f = state {screen = TextScreen str (f state)}

startSelectScreen :: AppState -> [String] -> Int -> SelectScreenFunc -> AppState
startSelectScreen state list idx f = state {screen = SelectScreen list idx (f state)}

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
    , Nothing <- (findCardIndexByCoords (Just coln) (Just $ (cards state) !! coln) x y 0)
    , None <- screen state
    = startCreatingCard state coln x y

    -- Start moving a card on left clicking it
    | EventKey (MouseButton LeftButton) Down _ (x, y) <- event
    , Just newState <- (startMovingCard state x y) -- this feels illegal
    , ((isNothing $ editing state) && (isNothing $ moving state))
    = newState

    -- Stop moving the card
    | EventKey (MouseButton LeftButton) Up _ (x, y) <- event
    , AppState cards (MoveScreen mov) a b c <- state
    , Just coln <- (findColumnByCoords 0 x y)
    , cardn <- (findCardIndexByCoords (Just coln) (Just $ cards !! coln) x y 0)
    = state {cards = (addCardTo cards coln cardn (movingCard mov)), screen = None}

    -- Start editing a card on right clicking it
    | EventKey (MouseButton RightButton) Down _ (x, y) <- event
    , Just newState <- (startEditingCard state x y)
    , ((isNothing $ editing state) && (isNothing $ moving state))
    = newState

    --
    -- Editing view
    --

    -- Stop editing on Q
    | EventKey (Char 'q') Down _ _ <- event
    , AppState cards (EditScreen edi) a b c <- state
    = state {cards = (addCardTo cards (movingPrevCol edi) (Just $ movingPrevPos edi) (movingCard edi)), screen = None}

    -- Remove card on D
    | EventKey (Char 'd') Down _ _ <- event
    , AppState cards (EditScreen edi) a b c <- state
    = state {screen = None}

    -- Handle mouse clicks in editing view
    | EventKey (MouseButton LeftButton) Up _ (x, y) <- event
    , EditScreen _ <- screen state = handleEditClick state x y

    -- Update debug info in Kanban view
    | EventMotion (x, y) <- event
    , ((isNothing $ editing state) && (isNothing $ moving state))
    = let coln = findColumnByCoords 0 x y
          col = indexColumn coln (cards state)
      in state {_mouse = (x,y), _column = coln, _card = (findCardByCoords coln col x y)}

    -- Update debug info in Editing view
    | EventMotion (x, y) <- event
    , EditScreen mcard <- (screen state)
    = state {_mouse = (x, y)}

    --
    -- Text view
    --

    -- Handle key presses in Text view
    | EventKey (Char c) Down _ _ <- event
    , TextScreen str f <- screen state
    = state {screen = TextScreen (str ++ [c]) f}

    -- Remove latest symbol on pressing Delete in Text view
    | EventKey (SpecialKey KeyDelete) Down _ _ <- event
    , TextScreen str f <- screen state
    = state {screen = TextScreen (pop str) f}

    -- Submit the result on pressing right arrow in Text view
    | EventKey (SpecialKey KeyRight) Down _ _ <- event
    , TextScreen str f <- screen state
    = f str

    --
    -- Select view
    --

    -- Submit the result on pressing down arrow in Text view
    | EventKey (SpecialKey KeyDown) Down _ _ <- event
    , SelectScreen list idx f <- screen state
    = f list idx

    -- Cycle through choices on pressing right arrow
    | EventKey (SpecialKey KeyRight) Down _ _ <- event
    , SelectScreen list idx f <- screen state
    , newidx <- idx + 1
    , ((length list) > newidx)
    = state {screen = SelectScreen list newidx f}

    -- Cycle back through choices on pressing left arrow
    | EventKey (SpecialKey KeyLeft) Down _ _ <- event
    , SelectScreen list idx f <- screen state
    , newidx <- idx - 1
    , (newidx >= 0)
    = state {screen = SelectScreen list newidx f}

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
      defaultState = AppState {
        cards = [[(Card "Test Card 1" 0 Medium), (Card "Test Card 2" 0 Medium)], [(Card "Test Card 3" 0 Medium), (Card "Test Card 4" 0 Medium)], [(Card "Test Card 5" 0 Medium)]]
        , screen = None
        , _mouse = (0,0)
        , _column = Nothing
        , _card = Nothing
      }