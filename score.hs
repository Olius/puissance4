module Game where

import Data.Tuple
import Data.List
import Data.Functor
import Data.Monoid
import Data.Array
import Data.Maybe
import Data.Tree
import qualified Data.Map.Strict as Map

data Player = AI | Human deriving (Enum, Eq, Ord)
instance Show Player where
        show AI = "A"
        show Human = "H"
instance Cyclic Player where
        next Human = AI
        next x = succ x

type Row = Int
type Col = Int
newtype Board player = Board (Array (Row,Col) (Maybe player))
rows = nub . map fst . indices
cols = nub . map snd . indices

instance Show player => Show (Board player) where
        show (Board a) =
                unlines [unwords [chip $ a!(i,j) | j <- cols a] | i <- rows a]
                where chip = maybe "." show

emptyBoard :: Row -> Col -> Board Player
emptyBoard m n = Board $ listArray ((1,1),(m,n)) $ repeat Nothing

data Puissance4State player = State
        { turn :: player
        , board :: Board player
        }

instance Show player => Show (Puissance4State player) where
        show = show . board

play :: Board player -> player -> Col -> Maybe (Board player)
play (Board a) p col = do
        top <- find isEmpty $ reverse $ rows a
        return $ Board $ a//[( (top,col), Just p )]
        where isEmpty row = isNothing $ a!(row,col)

class Cyclic a where
        next :: a -> a

moves :: Cyclic player => Puissance4State player -> [Puissance4State player]
moves (State { turn = t, board = board@(Board a)}) = map state boards where
        boards = mapMaybe (play board t) $ cols a
        state b = State { turn = next t, board = b}

moveTree :: Cyclic player =>
        Puissance4State player -> Tree (Puissance4State player)
moveTree = unfoldTree (\b -> (b, moves b))

printTree :: Show a => Tree a -> IO ()
printTree = putStr . drawTree . fmap show

-- type Score = Rational
-- data Scores player = Scores (Map.Map player Score) deriving Show
-- (!!!) :: Ord player => Map.Map player Score -> player -> Score
-- scores !!! player = Map.findWithDefault 0 player scores
-- 
-- totalScore :: Ord player => [Scores player] -> player -> Scores player
-- totalScore ss p = Scores $ Map.unionsWith (+) $
--                 [ fmap (((s!!!p)/playerScore) *) s | Scores s <- ss ]
--         where playerScore = sum [ s!!!p | Scores s <- ss ]

type Scores = Map.Map
(!!!) :: (Ord player, Num score) => Scores player score -> player -> score
s !!! p = Map.findWithDefault 0 p s

scoreSum :: (Ord player, Fractional field, Eq field) =>
        player -> [Scores player field] -> Scores player field
scoreSum p ss = Map.unionsWith (+) [ (s!!!p / pSum *) <$> s | s <- ss ]
        where pSum = let t = sum [ s!!!p | s <- ss ] in
                if t == 0 then 1 else t

horis rows cols n = do
        i <- [1..rows]
        j <- [1..cols-n+1]
        return [ (i,j+dj) | dj <- [0..n-1] ]
verts rows cols n = (swap <$>) <$> horis cols rows n
diags rows cols n = do
        is <- [[1..n]]
        js <- [[1..n],[n,n-1..1]]
        di <- [0..rows-n]; dj <- [0..cols-n]
        return $ ((+di) <$> is) `zip` ((+dj) <$> js)
     -- return [ (i+di,j+dj) | i <- is, j <- js ]

runs :: Int -> Array (Row,Col) e -> [[(Row,Col)]]
runs n a = horis r c n ++ verts r c n ++ diags r c n where
        (_,(r,c)) = bounds a

aligns :: Eq player => Int -> Board player -> [player]
aligns n (Board b) = [ p
        | is <- runs n b
        , let (c:cs) = [b!i|i<-is]
        , isJust c
        , let Just p = c
        , all (== c) cs
        ]

n = 3
treeAcc :: (Ord player, Fractional score, Eq score)
        => Scores player score
        -> Puissance4State player
        -> [Scores player score]
        -> Scores player score
treeAcc def State { turn=t, board=board } ss
        | null as = if null ss then def else scoreSum t ss
        | otherwise = Map.fromList [ (p, 1/fromIntegral (length ps)) | p <- ps ]
        where as = aligns n board; ps = nub as
