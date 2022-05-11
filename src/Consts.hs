module Consts where

columnAmount :: Int
columnAmount = 3

-- it might be better to have a dedicated User type
users :: [String]
users = ["Undefined", "Bob", "Alice", "John", "Valera"]

width, height, topSide, leftSide, textScale :: Float
width = 1200
height = 700
topSide = height/2
leftSide = -width/2
textScale = 0.2

editCardNameFieldXStart, editCardNameFieldYStart, editCardNameFieldYEnd :: Float
editCardNameFieldXStart = leftSide + 20
editCardNameFieldYStart = topSide - 60
editCardNameFieldYEnd = topSide - 25

editCardWorkerFieldXStart, editCardWorkerFieldYStart, editCardWorkerFieldYEnd :: Float
editCardWorkerFieldXStart = leftSide + 20
editCardWorkerFieldYStart = topSide - 110
editCardWorkerFieldYEnd = topSide - 70

editCardNameTip, editCardPersonTip, editCardExitTip, editCardRemoveTip :: String
editCardNameTip = "Name:"
editCardPersonTip = "Person:"
editCardExitTip = "Press Q to save & exit"
editCardRemoveTip = "Press D to remove the card"

textInputTip, textInputControlsTip :: String
textInputTip = "Enter the new value:"
textInputControlsTip = "Delete to remove last letter, right arrow to submit"

selectScreenTip, selectScreenControlsTip :: String
selectScreenTip = "Pick the new value:"
selectScreenControlsTip = "Use left/right arrows to pick, down arrow to submit"