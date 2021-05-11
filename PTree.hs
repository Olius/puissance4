module PTree where

import Data.Tree

import State

allMoves :: [Move]
allMoves = [ (p,c) | p <- [minBound..], c <- [0..] ]

fullTree :: Forest Move
fullTree = unfoldForest (\m -> (m, allMoves)) allMoves

nTree :: ICol -> Forest Move
nTree n = [ Node (p,c) $ nTree n | p <- [minBound..] , c <- [0..n-1] ]
