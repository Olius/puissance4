module Tree where

import Data.Array
import Data.Maybe
import Data.Tree

import Game

playTree :: State -> Tree State
playTree = unfoldTree tuple where
        tuple s@(State p b) = ( s, mapMaybe (flip playTurn s) moves ) where
                moves = range (c0,c1)
                ((_,c0),(_,c1)) = bounds b

trim :: Int -> Tree a -> Tree a
trim 0 (Node a _ ) = Node a []
trim n (Node a as) = Node a $ trim (n-1) <$> as
