module AStar ( astar
             , Goal(Goal)
             , Start(Start)
             , Solver(..)
             ) where

import qualified Data.Heap   as H
import           Data.Monoid      ((<>), mempty)
import qualified Data.Set    as S

newtype Start coord = Start coord

newtype Goal coord = Goal coord
                         deriving Eq

data Step coord cost = Step { getPathCost      :: !cost
                            , getHeuristicCost :: !cost
                            , getCoord         :: !coord
                            , getPredecessor   :: !(Maybe (Step coord cost))
                            } deriving (Eq, Show)

instance (Eq coord, Ord coord, Monoid c, Eq c, Ord c) => Ord (Step coord c) where
    Step p1 h1 c1 pr1 <= Step p2 h2 c2 pr2 = (p1 <> h1, c1, pr1) <= (p2 <> h2, c2, pr2)

data Solver coord cost = Solver { getStart  :: Start coord
                                , getGoal   :: Goal coord
                                , heuristic :: coord -> Goal coord -> cost
                                , expand    :: cost -> coord -> [(coord, cost)]
                                }

astar :: (Eq coord, Ord coord, Monoid cost, Eq cost, Ord cost)
      => Solver coord cost
      -> Maybe [coord]
astar solver = extractPath [] <$> findPath solver

extractPath :: [coord] -> Step coord cost -> [coord]
extractPath acc step =
    let acc' = getCoord step : acc
    in case getPredecessor step of
            Just step' -> extractPath acc' step'
            Nothing    -> acc'

findPath :: (Ord cost, Ord coord, Monoid cost)
         => Solver coord cost
         -> Maybe (Step coord cost)
findPath solver = do 

    let (Start start) = getStart solver

    let step0 = Step { getPathCost      = mempty
                     , getHeuristicCost = heuristic solver start (getGoal solver)
                     , getCoord         = start
                     , getPredecessor   = Nothing
                     }

    go (H.singleton step0) S.empty
    where
    go fringe done = do

        (f, ringe) <- H.uncons fringe

        let coord = getCoord f

        if Goal coord == getGoal solver
            then Just f
            else do

                let costedCandidates = filter (\(nc, _) -> not $ S.member nc done)
                                     . expand solver (getPathCost f)
                                     $ coord

                let nextSteps = H.fromList
                                . map (\(nc, pc) -> Step pc (heuristic solver nc (getGoal solver)) nc (Just f))
                                $ costedCandidates

                go (ringe <> nextSteps) (S.insert coord done)
