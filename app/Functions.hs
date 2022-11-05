{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# LANGUAGE OverloadedStrings #-}

module Functions
  ( checkRow
  , checkBoardRow
  , Board
  , Evaluation
  , initialBoard
  , initialEvaluation
  , updateEvaluation
  , updateBoard
  , colors
  ) where

import           Data.Array  as A (Array, Ix, array, (!), (//))
import           Data.Map    as M (Map, empty, fromList, insert, intersection,
                                   mapWithKey, (!))
import qualified Data.Set    as Set
import           Miso.String (MisoString)

type Board = Array Integer Integer

type Evaluation = Array Integer Integer

colors :: M.Map Integer MisoString
colors =
  M.fromList
    [ (0, "white")
    , (1, "blue")
    , (2, "green")
    , (3, "red")
    , (4, "orange")
    , (5, "yellow")
    , (6, "black")
    , (7, "brown")
    , (8, "magenta")
    ]

initialBoard :: Array Integer Integer
initialBoard = array (1, 40) [(i, 0) | i <- [1 .. 40]]

initialEvaluation :: Array Integer Integer
initialEvaluation = array (1, 20) [(i, 0) | i <- [1 .. 20]]

updateEvaluation :: (Ix a, Num a) => Array a e -> a -> e -> e -> Array a e
updateEvaluation ev row r w = ev // [(2 * row - 1, r), (2 * row, w)]

updateBoard :: (Ix i, Num i) => Array i e -> i -> i -> e -> Array i e
updateBoard board x y v = board // [((y - 1) * 4 + x, v)]

checkRow :: [Integer] -> [Integer] -> (Integer, Integer)
checkRow row state = (toInteger r, toInteger w)
  where
    (acc1, acc2) = removeIdentic row state
    counts1 = counts acc1
    counts2 = counts acc2
    m = intersection counts1 counts2
    keys = mapWithKey (\k _ -> min (counts1 M.! k) (counts2 M.! k)) m
    r = sum keys
    w =
      fromEnum (head row == head state) + fromEnum (row !! 1 == state !! 1) +
      fromEnum (row !! 2 == state !! 2) +
      fromEnum (row !! 3 == state !! 3)

checkBoardRow ::
     Array Integer Integer -> Integer -> [Integer] -> (Integer, Integer)
checkBoardRow board rowNumber state = (toInteger r, toInteger w)
  where
    row =
      [ board A.! (4 * rowNumber - 3)
      , board A.! (4 * rowNumber - 2)
      , board A.! (4 * rowNumber - 1)
      , board A.! (4 * rowNumber)
      ]
    (r, w) = checkRow row state

removeIdenticHelper ::
     [Integer] -> [Integer] -> [Integer] -> [Integer] -> ([Integer], [Integer])
removeIdenticHelper list1 list2 acc1 acc2 =
  if null list1 && null list2
    then (acc1, acc2)
    else removeIdenticHelper (tail list1) (tail list2) new_acc1 new_acc2
  where
    x = head list1
    y = head list2
    new_acc1 =
      if x /= y
        then x : acc1
        else acc1
    new_acc2 =
      if x /= y
        then y : acc2
        else acc2

removeIdentic :: [Integer] -> [Integer] -> ([Integer], [Integer])
removeIdentic list1 list2 = removeIdenticHelper list1 list2 [] []

countsReq :: Ord k => [k] -> [k] -> Map k Int -> Map k Int
countsReq l1 l2 m =
  if null l1
    then m
    else countsReq (tail l1) l2 (insert (head l1) c m)
  where
    c = count (head l1) l2

counts :: Ord k => [k] -> Map k Int
counts list = countsReq (Set.toList s) list M.empty
  where
    s = Set.fromList list

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)
