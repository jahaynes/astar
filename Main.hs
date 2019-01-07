{-# LANGUAGE TupleSections #-}

module Main where

import AStar

import           Data.Monoid ((<>))
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

type Grid a = Vector (Vector a)

grid :: Vector (Vector Char)
grid = V.fromList . map V.fromList
     $ [ "***********"
       , "*g        *"
       , "***** *** *"
       , "    ****  *"
       , "    *     *"
       , "    * s   *"
       , "    *******"
       ]

find :: Eq a => a -> Grid a -> Maybe Coord
find item = goRow 0
    where
    goRow i rows
        | V.null rows = Nothing
        | otherwise =
            case goCol 0 (V.head rows) of
                Just ij -> Just ij
                Nothing -> goRow (i+1) (V.tail rows)
            where
            goCol j cols
                | V.null cols         = Nothing
                | V.head cols == item = Just $ Coord (i,j)
                | otherwise           = goCol (j+1) (V.tail cols)

data Cost a = Cost a
                   deriving (Eq, Ord)

instance Num a => Monoid (Cost a) where
    mappend (Cost a) (Cost b) = Cost(a + b)
    mempty = Cost 0

newtype Coord = Coord (Int, Int)
                    deriving (Eq, Ord, Show)

main :: IO ()
main = do

    let (Just start) = Start <$> find 's' grid

        (Just goal)  = Goal  <$> find 'g' grid

        solver = Solver { getStart  = start
                        , getGoal   = goal
                        , heuristic = dist
                        , expand    = expansion
                        }

    print $ astar solver

    where
    dist :: Num a => Coord -> Goal Coord -> Cost a
    dist (Coord(a,b)) (Goal(Coord(c,d))) = Cost(fromIntegral $ abs(a-c)) <> Cost(fromIntegral $ abs(b-d))

    expansion :: Enum a => Cost a -> Coord -> [(Coord, Cost a)]
    expansion pc (Coord (ci,cj)) = map (, inc pc)
                                 . filter check
                                 $ map Coord [ (ci+1, cj  )
                                             , (ci  , cj+1)
                                             , (ci-1, cj  )
                                             , (ci  , cj-1)
                                             ]
        where
        inc :: Enum a => Cost a -> Cost a
        inc (Cost a) = Cost (succ a)

        check :: Coord -> Bool
        check (Coord (i,j)) = i >= 0
                           && i < V.length grid
                           && j >= 0
                           && j < V.length (grid ! i)
                           && grid ! i ! j /= '*'
