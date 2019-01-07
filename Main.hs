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

newtype Cost = Cost Float
                   deriving (Eq, Ord)

instance Monoid Cost where
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
    dist :: Coord -> Goal Coord -> Cost
    dist (Coord(a,b)) (Goal(Coord(c,d))) = Cost(fromIntegral $ abs(a-c)) <> Cost(fromIntegral $ abs(b-d))

    expansion :: Cost -> Coord -> [(Coord, Cost)]
    expansion pc (Coord (ci,cj)) = map (, inc pc)
                                 . filter check
                                 $ map Coord [ (ci+1, cj  )
                                             , (ci  , cj+1)
                                             , (ci-1, cj  )
                                             , (ci  , cj-1)
                                             ]
        where
        inc :: Cost -> Cost
        inc (Cost a) = Cost (a+1)

        check :: Coord -> Bool
        check (Coord (i,j)) = i >= 0
                           && i < V.length grid
                           && j >= 0
                           && j < V.length (grid ! i)
                           && grid ! i ! j /= '*'
