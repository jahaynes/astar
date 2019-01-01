import           Data.Heap   (Heap)
import qualified Data.Heap                 as H
import           Data.Maybe  (mapMaybe)
import           Data.Set    (Set)
import qualified Data.Set                  as S
import           Data.Vector (Vector, (!))
import qualified Data.Vector               as V

type Grid a = Vector (Vector a)

globalGrid :: Vector (Vector Char)
globalGrid = V.fromList . map V.fromList
     $ [ "***********"
       , "*g        *"
       , "***** *** *"
       , "    ****  *"
       , "    *     *"
       , "    * s   *"
       , "    *******"
       ]

find :: Eq a => a -> Grid a -> Maybe (Int, Int)
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
                | V.head cols == item = Just (i,j)
                | otherwise           = goCol (j+1) (V.tail cols)

data Step = Step { getPathCost      :: !Int
                 , getHeuristicCost :: !Int
                 , getCoord         :: !(Int, Int)
                 , getPredecessor   :: !(Maybe Step)
                 } deriving (Eq, Show)

instance Ord Step where
    Step p1 h1 c1 pr1 <= Step p2 h2 c2 pr2 = (p1+h1, c1, pr1) <= (p2+h2, c2, pr2)

main :: IO ()
main = do
    let (Just start) = find 's' globalGrid
        (Just goal)  = find 'g' globalGrid
    print $ astar globalGrid start goal

astar :: Grid Char -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
astar grid start goal = do

    let step0 = Step { getPathCost      = 0
                     , getHeuristicCost = dist start goal
                     , getCoord         = start
                     , getPredecessor   = Nothing
                     }

    extractPath [] <$> findPath (H.singleton step0) S.empty

    where
    extractPath :: [(Int, Int)] -> Step -> [(Int, Int)]
    extractPath acc step =
        let acc' = getCoord step : acc
        in case getPredecessor step of
              Just step' -> extractPath acc' step'
              Nothing    -> acc'

    findPath :: Heap Step -> Set (Int, Int) -> Maybe Step
    findPath fringe done = do

        (s@(Step pc _ c@(i,j) _), fs) <- H.uncons fringe

        if c == goal
            then Just s
            else do
                let nextCoords = mapMaybe (check done) [ (i+1, j  )
                                                       , (i  , j+1)
                                                       , (i-1, j  )
                                                       , (i  , j-1)
                                                       ]
                    nextSteps = H.fromList
                              . map (\nc -> Step (pc+1) (dist nc goal) nc (Just s))
                              $ nextCoords

                findPath (H.union fs nextSteps) (S.insert c done)

    dist :: (Int, Int) -> (Int, Int) -> Int
    dist (a,b) (c,d) = abs (a-c) + abs (b-d)

    check :: Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
    check done (i,j)
        | i <  0                   = Nothing
        | i >= V.length grid       = Nothing
        | j <  0                   = Nothing
        | j >= V.length (grid ! i) = Nothing
        | (i,j) `S.member` done    = Nothing
        | grid ! i ! j == '*'      = Nothing
        | otherwise                = Just (i, j)
