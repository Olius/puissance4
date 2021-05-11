module Board where

import State
import Control.Applicative

type Chip = Player
type Row = [Chip]
type Col = [Chip]
newtype Board = Board { getBoard :: ZipList Col }
        deriving Show

instance Semigroup Board where
        (Board cs) <> (Board ds) = Board $ liftA2 (<>) cs ds
instance Monoid Board where
        mempty = Board $ ZipList $ repeat []

move2Board :: Move -> Board
move2Board (p,c) = Board $ ZipList $ replicate c mempty ++ [[p]] ++ repeat mempty

state2Board :: State -> Board
state2Board = foldMap move2Board
