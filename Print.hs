module Print where

import Data.Array

import Game

drawBoard :: Board -> String
drawBoard b = reverse $ unlines [ unwords [ disp $ b!(i,j) | j <- range (j0,j1) ]
                                | i <- range (i0,i1)
                                ]
        where disp (Just AI)    = "A"
              disp (Just Human) = "H"
              disp Nothing      = "."
              ((i0,j0),(i1,j1)) = bounds b
