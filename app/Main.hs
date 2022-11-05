-- | Haskell language pragma
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Haskell module declaration
module Main where

import           Styles
import           System.Random

import           Data.Array                       as A ((!), (//))

-- | Miso framework import
import           Miso

import           Functions                        (Board, Evaluation, colors,
                                                   initialBoard,
                                                   initialEvaluation)
import           Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080

data GameModel =
  Game
    { currentRow  :: Integer
    , pickedColor :: Integer
    , state       :: [Integer]
    , board       :: Board
    , evaluation  :: Evaluation
    }
  deriving (Show, Eq)

-- | Type synonym for an application model
type Model = GameModel

-- | Sum type for application events
data Action
  = PickColor Integer
  | AssignColor Integer
  | NoOp
  deriving (Show, Eq)

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
main =
  runApp $ do
    g <- getStdGen
    let state = generateState g
    startApp
      App
        { initialAction = NoOp -- initial action to be executed on application load
        , model =
            Game
              { currentRow = 10
              , state = state
              , board = initialBoard
              , evaluation = initialEvaluation
              , pickedColor = 0
              } -- initial model
        , update = updateModel -- update function
        , view = viewModel -- view function
        , events = defaultEvents -- default delegated events
        , subs = [] -- empty subscription list
        , mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
        , logLevel = Off
        }

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (PickColor i) m = noEff (m {pickedColor = i})
updateModel (AssignColor i) m = noEff m {board = newBoard}
  where
    b = board m
    row = currentRow m
    ri =
      case mod i 4 of
        0 -> div i 4
        1 -> div (i + 3) 4
        2 -> div (i + 2) 4
        _ -> div (i + 1) 4
    pc = pickedColor m
    newBoard =
      if ri == row
        then b A.// [(i, pc)]
        else b

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m =
  div_
    [class_ "content"]
    [div_ [style_ boardStyle] rows, div_ [style_ boardStyle] colorsForPick]
  where
    b = board m
    n = length b
    rowCount = div n 4
    colorsForPick =
      [ div_ [getNStyle i colors, style_ cellStyle, onClick (PickColor i)] []
      | i <- [1 .. 8]
      ]
    rows =
      [ div_
        []
        [ div_
            [ style_ cellStyle
            , getNStyle (b A.! toInteger (4 * x - 3)) colors
            , onClick (AssignColor (toInteger (4 * x - 3)))
            ]
            []
        , div_
            [ style_ cellStyle
            , getNStyle (b A.! toInteger (4 * x - 2)) colors
            , onClick (AssignColor (toInteger (4 * x - 2)))
            ]
            []
        , div_
            [ style_ cellStyle
            , getNStyle (b A.! toInteger (4 * x - 1)) colors
            , onClick (AssignColor (toInteger (4 * x - 1)))
            ]
            []
        , div_
            [ style_ cellStyle
            , getNStyle (b A.! toInteger (4 * x)) colors
            , onClick (AssignColor (toInteger (4 * x)))
            ]
            []
        ]
      | x <- [1 .. rowCount]
      ]
