import           Data.Heap   (Heap)
import qualified Data.Heap            as H
import           Data.Maybe  (mapMaybe)
import           Data.Set    (Set)
import qualified Data.Set             as S
import           Data.Vector (Vector, (!))
import qualified Data.Vector          as V

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

dist :: (Int, Int) -> (Int, Int) -> Int
dist (a,b) (c,d) = abs (a-c) + abs (b-d)

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
                 } deriving Show

instance Eq Step where
    Step _ _ c1 == Step _ _ c2 = c1 == c2
                 
instance Ord Step where
    Step p1 h1 c1 <= Step p2 h2 c2 = (p1+h1, c1) <= (p2+h2, c2)

main :: IO ()
main = do
    let (Just start) = find 's' globalGrid
        (Just goal)  = find 'g' globalGrid
    print (start, goal)
    print =<< astar globalGrid start goal
    return ()

astar :: Grid Char -> (Int, Int) -> (Int, Int) -> IO Bool
astar grid start goal = do

    let step0 = Step { getPathCost      = 0
                     , getHeuristicCost = dist start goal
                     , getCoord         = start
                     }

    go (H.singleton step0) S.empty

    where
    go :: Heap Step -> Set (Int, Int) -> IO Bool
    go fringe done =
        case H.uncons fringe of
            Nothing -> return False
            Just (Step pc _ c@(i,j), fs) ->
                if c == goal
                    then return True
                    else do
                        print (pc, c)
                        let done' = S.insert c done
                            nextCoords = mapMaybe (check grid done) [ (i+1, j  )
                                                                    , (i  , j+1)
                                                                    , (i-1, j  )
                                                                    , (i  , j-1)
                                                                    ]
                            nextSteps = H.fromList . map (\nc -> Step (pc+1) (dist nc goal) nc) $ nextCoords
                        go (H.union fs nextSteps) done'

check :: Grid Char -> Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
check grid done (i,j)
    | i <  0                   = Nothing
    | i >= V.length grid       = Nothing
    | j <  0                   = Nothing
    | j >= V.length (grid ! i) = Nothing
    | (i,j) `S.member` done    = Nothing
    | grid ! i ! j == '*'      = Nothing
    | otherwise                = Just (i, j)
