-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Haskell module declaration
module Main where

import Styles
import System.Random

-- | Miso framework import
import Miso
import Miso.String (MisoString)
import Data.Foldable (toList)

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle
import Data.Sequence (mapWithIndex)

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

-- | Type synonym for an application model
type Model = (Int, [MisoString])

-- | Sum type for application events
data Action
  = ChangeColor Int
  | NoOp
  deriving (Show, Eq)

colors :: [MisoString]
colors = ["blue", "green", "red", "orange", "yellow", "black", "brown", "white"]

randomList :: StdGen -> Int -> [MisoString] -> [MisoString]
randomList g i lst =
  if i == 4
    then y : lst
    else randomList next_g (i + 1) (y : lst)
  where
    (x, next_g) = randomR (0, length colors) g
    y = colors !! x

generateState :: StdGen -> [MisoString]
generateState generator = randomList generator 1 []

-- | Entry point for a miso application
main :: IO ()
main = runApp $ do
  g <- getStdGen
  let state = generateState g
  startApp App {
    initialAction = NoOp -- initial action to be executed on application load
    , model  = (0, state)                  -- initial model
    , update = updateModel          -- update function
    , view   = viewModel            -- view function
    , events = defaultEvents        -- default delegated events
    , subs   = []                   -- empty subscription list
    , mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    , logLevel = Off
  }

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (ChangeColor i) (n, x) = noEff (i, x)

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel (n, x) = div_ [class_ "container"] [
   div_ [style_ rowStyle]
     divs
   ]
   where
    colorsWithInds = zip [0..] x
    divs = (map  (\(i,c) -> div_ [style_ circleStyle, if n == i then style_ aquaStyle else (getStyle c), style_ blockStyle, onClick (ChangeColor i)] []) colorsWithInds)

