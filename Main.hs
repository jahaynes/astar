{-# LANGUAGE TupleSections #-}

module Main where

import AStar

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

instance Monoid Int where
    mappend a b = a + b
    mempty = 0

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
    dist :: Coord -> Goal Coord -> Int
    dist (Coord (a,b)) (Goal (Coord (c,d))) = abs (a-c) + abs (b-d)

    expansion :: Int -> Coord -> [(Coord, Int)]
    expansion pc (Coord (ci,cj)) = map (, succ pc)
                                 . filter check
                                 $ map Coord [ (ci+1, cj  )
                                             , (ci  , cj+1)
                                             , (ci-1, cj  )
                                             , (ci  , cj-1)
                                             ]
        where
        check (Coord (i,j)) = i >= 0
                           && i < V.length grid
                           && j >= 0
                           && j < V.length (grid ! i)
                           && grid ! i ! j /= '*'
