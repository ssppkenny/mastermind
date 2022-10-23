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
import Miso.String

import Control.Monad.IO.Class

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

-- | Type synonym for an application model
type Model = [MisoString]

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | ChangeColor Int
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)



colors :: [MisoString]
colors = ["blue", "green", "red", "orange", "yellow", "black", "brown", "white"]

randomList :: StdGen -> Int -> [MisoString] -> [MisoString]
randomList g i lst =
  if i == 4
    then y : lst
    else randomList next_g (i + 1) (y : lst)
  where
    (x, next_g) = (randomR (0, Prelude.length colors) g)
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
    , model  = state                  -- initial model
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
updateModel (ChangeColor i) m = noEff m 

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [class_ "container"] [
   div_ [style_ rowStyle]
    (Prelude.map (\c -> div_ [style_ circleStyle, (getStyle c), style_ blockStyle, onClick (ChangeColor 0)] []) x)
   ]

