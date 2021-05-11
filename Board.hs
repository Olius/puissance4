module Board where

import State

type Chip = Player
type Row = [Chip]
type Col = [Chip]
newtype Board = Board { getBoard :: [Col] }
        deriving Show

instance Semigroup Board where
        (Board cs) <> (Board ds) = Board $ zipWith (++) cs ds
instance Monoid Board where
        mempty = Board $ repeat []

move2Board :: Move -> Board
move2Board (p,c) = Board $ replicate c mempty ++ [[p]] ++ repeat mempty

state2Board :: State -> Board
state2Board = foldMap move2Board
