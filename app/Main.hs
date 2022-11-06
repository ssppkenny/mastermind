-- | Haskell language pragma
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | Haskell module declaration
module Main where

import           Styles

import           System.Random                    (getStdGen)

import           Data.Array                       as A ((!), (//))

-- | Miso framework import
import           Miso

import           Functions                        (Board, Evaluation,
                                                   checkBoardRow, colors,
                                                   generateState, initialBoard,
                                                   initialEvaluation,
                                                   integerToMisoString,
                                                   isRowFull, updateEvaluation)
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
    , showState   :: Bool
    }
  deriving (Show, Eq)

-- | Type synonym for an application model
type Model = GameModel

-- | Sum type for application events
data Action
  = PickColor Integer
  | AssignColor Integer
  | CheckCurrentRow
  | NoOp
  deriving (Show, Eq)

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
              , showState = False
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
updateModel CheckCurrentRow m = noEff newModel
  where
    b = board m
    s = state m
    cr = currentRow m
    (r, w) = checkBoardRow b cr s
    ev = evaluation m
    newEvaluation = updateEvaluation ev cr r w
    newModel =
      m
        { evaluation = newEvaluation
        , currentRow =
            if cr >= 2 && isRowFull b cr
              then cr - 1
              else cr
        , showState = cr == 1 || r == 4 && w == 0
        }
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
    [ div_
        [ style_ boardStyle
        , if showStateLine
            then style_ stateVisibleStyle
            else style_ stateInvisibleStyle
        ]
        [ div_ [style_ cellStyle, (getNStyle (s !! 0) colors)] []
        , div_ [style_ cellStyle, (getNStyle (s !! 1) colors)] []
        , div_ [style_ cellStyle, (getNStyle (s !! 2) colors)] []
        , div_ [style_ cellStyle, (getNStyle (s !! 3) colors)] []
        ]
    , div_ [style_ boardStyle, style_ borderStyle] rows
    , div_ [style_ boardStyle, style_ borderStyle] colorsForPick
    , div_
        [style_ boardStyle]
        [ button_
            [ style_ widthStyle
            , if isRowFull b cr
                then style_ stateVisibleStyle
                else style_ stateInvisibleStyle
            , onClick CheckCurrentRow
            ]
            [text "Check row"]
        ]
    ]
  where
    b = board m
    n = length b
    s = state m
    cr = currentRow m
    showStateLine = showState m
    rowCount = div n 4
    pc = pickedColor m
    ev = evaluation m
    colorsForPick =
      [ div_
        [ getNStyle i colors
        , if pc == i
            then style_ selectedCellStyle
            else style_ cellStyle
        , onClick (PickColor i)
        ]
        []
      | i <- [1 .. 8]
      ]
    rows =
      [ div_
        [ if toInteger x == cr
            then style_ currentRowStyle
            else style_ emptyStyle
        ]
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
        , div_
            [style_ evStyle]
            [ span_
                []
                [text $ integerToMisoString (ev A.! toInteger (2 * x - 1))]
            , span_ [] [text $ integerToMisoString (ev A.! toInteger (2 * x))]
            ]
        ]
      | x <- [1 .. rowCount]
      ]
