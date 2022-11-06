{-# LANGUAGE OverloadedStrings #-}

module Styles where

import qualified Data.Map    as M
import           Miso
import           Miso.String

getStyle :: MisoString -> Attribute action
getStyle c = style_ (M.fromList [("background", c)])

getNStyle :: Integer -> M.Map Integer MisoString -> Attribute action
getNStyle n colors = style_ (M.fromList [("background", colors M.! n)])

boardStyle :: M.Map MisoString MisoString
boardStyle =
  M.fromList
    [ ("overflow", "hidden")
    , ("width", "200px")
    , ("left", "0")
    , ("right", "0")
    , ("top", "0")
    , ("bottom", "0")
    , ("margin", "auto")
    ]

cellStyle :: M.Map MisoString MisoString
cellStyle =
  M.fromList
    [ ("width", "20px")
    , ("height", "20px")
    , ("border-radius", "20px")
    , ("float", "left")
    , ("margin", "10px")
    , ("border-color", "black")
    , ("border", "1px solid")
    ]

stateInvisibleStyle :: M.Map MisoString MisoString
stateInvisibleStyle = M.fromList [("visibility", "hidden")]

borderStyle :: M.Map MisoString MisoString
borderStyle = M.fromList [("border", "1px solid")]

widthStyle :: M.Map MisoString MisoString
widthStyle = M.fromList [("width", "200px")]

stateVisibleStyle :: M.Map MisoString MisoString
stateVisibleStyle =
  M.fromList [("visibility", "visible"), ("border", "1px solid")]

selectedCellStyle :: M.Map MisoString MisoString
selectedCellStyle =
  M.fromList
    [ ("width", "20px")
    , ("height", "20px")
    , ("border-radius", "20px")
    , ("float", "left")
    , ("margin", "9px")
    , ("border-color", "red")
    , ("border", "2px solid")
    ]

evStyle :: M.Map MisoString MisoString
evStyle =
  M.fromList [("width", "20px"), ("float", "left"), ("margin-top", "10px")]

blockStyle :: M.Map MisoString MisoString
blockStyle = M.fromList [("width", "20px"), ("float", "left")]

rowStyle :: M.Map MisoString MisoString
rowStyle = M.fromList [("width", "220px")]

redStyle :: M.Map MisoString MisoString
redStyle = M.fromList [("background", "red")]

greenStyle :: M.Map MisoString MisoString
greenStyle = M.fromList [("background", "green")]

blueStyle :: M.Map MisoString MisoString
blueStyle = M.fromList [("background", "blue")]

yellowStyle :: M.Map MisoString MisoString
yellowStyle = M.fromList [("background", "yellow")]

aquaStyle :: M.Map MisoString MisoString
aquaStyle = M.fromList [("background", "aqua")]

squareStyle :: M.Map MisoString MisoString
squareStyle =
  M.fromList
    [ ("width", "20px")
    , ("height", "20px")
    , ("border-radius", "0px")
    , ("margin", "10px")
    , ("border-width", "1px")
    , ("border-color", "black")
    , ("border", "solid")
    ]

circleStyle :: M.Map MisoString MisoString
circleStyle =
  M.fromList
    [ ("width", "20px")
    , ("height", "20px")
    , ("border-radius", "10px")
    , ("margin", "10px")
    , ("border-color", "black")
    ]
