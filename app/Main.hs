-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Haskell module declaration
module Main where

import Styles
import System.Random

import           Data.Map   as M (Map, fromList, (!))

-- | Miso framework import
import Miso
import Miso.String (MisoString)

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

data GameModel = Game { position :: Integer
                      , state :: [Integer]
} deriving (Show,Eq)

-- | Type synonym for an application model
type Model = GameModel

-- | Sum type for application events
data Action
  = ChangeColor Integer
  | NoOp
  deriving (Show, Eq)

colors :: M.Map Integer MisoString
colors = M.fromList [(1, "blue"), (2, "green"), (3,"red"), (4,"orange"), (5,"yellow"), (6,"black"), (7,"brown"), (8,"magenta")]

randomList :: StdGen -> Integer -> [Integer] -> [Integer]
randomList g i lst =
  if i == 4
    then toInteger x : lst
    else randomList next_g (i + 1) (toInteger x : lst)
  where
    (x, next_g) = randomR (0, length colors) g

generateState :: StdGen -> [Integer]
generateState generator = randomList generator 1 []



-- | Entry point for a miso application
main :: IO ()
main = runApp $ do
  g <- getStdGen
  let state = generateState g
  startApp App {
    initialAction = NoOp -- initial action to be executed on application load
    , model  = Game { position = -1,  state=state}                -- initial model
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
updateModel (ChangeColor i) m = noEff (Game {position=i, state=x})
      where x = state m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [class_ "container"] [
   div_ [style_ rowStyle]
     divs
   ]
   where
    n = position m
    x = state m
    colorsWithInds = map (\t -> (t, colors M.! t)) x
    divs = map  (\(i,c) -> div_ [style_ squareStyle,
     if n == i then style_ aquaStyle else getStyle c, style_ blockStyle, onClick (ChangeColor i)] []) colorsWithInds






