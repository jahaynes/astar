module Optimise where

import           Data.List (subsequences)

data Optimise coord cost = Optimise { getCost :: coord -> coord -> cost
                                    , isFree :: coord -> Bool }

data WholePath coord cost =
    WholePath { start   :: coord
              , middles :: [coord]
              , goal    :: coord
              } deriving Show

mkPath :: [coord] -> WholePath coord cost
mkPath (p:ps) = WholePath { start   = p
                          , middles = init ps
                          , goal    = last ps
                          }

removal :: WholePath coord cost -> [WholePath coord cost]
removal (WholePath start middles goal) =
    map (\middle -> WholePath start middle goal) (subsequences middles)


-- LINEARISE !!! take a path to <- THIS WONT WORK.  2x3 problem!
    
    
-- optimiseWith :: Optimise coord -> [coord] -> [coord]
optimiseWith (Optimise getCost isFree) ps = go [] (mkPath ps)
    where
    go acc wholePath = do

        mapM_ print $ removal wholePath

        
    pathCost :: WholePath coord cost -> cost
    pathCost (WholePath s ms g) =
        
        foldl' 
