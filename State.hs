module State where

class Cyclic a where
        next :: a -> a
data Player = A | B
        deriving (Show, Enum, Bounded)
instance Cyclic Player where
        next B = A
        next x = succ x

type IRow = Int
type ICol = Int

type Move  = (Player, ICol)
type State = [Move]
