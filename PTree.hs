module PTree where

import Data.Tree

import State

allMoves :: [Move]
allMoves = [ (p,c) | p <- [minBound..], c <- [0..] ]

fullTree :: Forest Move
fullTree = unfoldForest (\m -> (m, allMoves)) allMoves
