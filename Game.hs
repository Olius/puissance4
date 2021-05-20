module Game where

import Data.Array
import Data.List
import Data.Maybe

data Player = Human
            | AI
        deriving ( Eq
                 , Show
                 )

other :: Player -> Player
other Human = AI
other AI = Human

type ICol = Word
type IRow = Word
type NWin = Word

type Chip = Maybe Player

type Board = Array (IRow,ICol) Chip

data Move = Move { player :: Player
                 , column :: ICol
                 }

play :: Move -> Board -> Maybe Board
play (Move p c) b = do
        r <- listToMaybe [ i | i <- range (r0,r1), isNothing $ b!(i,c) ]
        return $ b//[((r,c), Just p)]
        where ((r0,_),(r1,_)) = bounds b

won :: NWin -> Board -> Player -> Bool  -- NWin -> Board -> [Player]
won n b = (`elem` winners) where
        winners = [ x | l <- lines, [(Just x:_)] <- [group $ (b!) <$> l] ]
        lines = horz ++ vert ++ diag ++ gaid
        horz = [ [ (i0,j) | j <- range (j0,j0+n-1) ]
                | i0 <- range (r0,r1)
                , j0 <- range (c0,c1-n+1)
                ]
        vert = [ [ (i,j0) | i <- range (i0,i0+n-1) ]
                | i0 <- range (r0,r1-n+1)
                , j0 <- range (c0,c1)
                ]
        diag = [ range (i0,i0+n-1) `zip` range (j0,j0+n-1)
                | i0 <- range (r0,r1-n+1)
                , j0 <- range (c0,c1-n+1)
                ]
        gaid = [ range (i0,i0+n-1) `zip` (reverse $ range (j0,j0+n-1))
                | i0 <- range (r0,r1-n+1)
                , j0 <- range (c0,c1-n+1)
                ]
        ((r0,c0),(r1,c1)) = bounds b

data State = State { turn :: Player
                   , board :: Board
                   }

playTurn :: ICol -> State -> Maybe State
playTurn c (State p b) = State (other p) <$> play (Move p c) b
