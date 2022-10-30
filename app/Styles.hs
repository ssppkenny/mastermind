{-# LANGUAGE OverloadedStrings #-}

module Styles where

import qualified Data.Map    as M
import           Miso
import           Miso.String

getStyle :: MisoString -> Attribute action
getStyle c = style_ (M.fromList [("background", c)])

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
