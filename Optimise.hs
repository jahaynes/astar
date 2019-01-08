module Optimise where

import          Data.Sequence       (Seq)
import qualified Data.Sequence as S

data Optimise coord = Optimise { isFree :: coord -> Bool }

data WholePath coord cost =
    WholePath { start   :: coord
              , middles :: Seq coord
              , goal    :: coord
              } deriving Show

mkPath :: [coord] -> WholePath coord cost
mkPath (p:ps) = WholePath { start   = p
                          , middles = S.fromList (init ps)
                          , goal    = last ps
                          }

removal :: WholePath coord cost -> [WholePath coord cost]
removal (WholePath start middles goal) = do

    idx <- [0..length middles]

    let middles' = S.deleteAt idx middles

    if S.null middles'

        then pure (WholePath start middles' goal)

        else removal (WholePath start middles' goal)

-- optimiseWith :: Optimise coord -> [coord] -> [coord]
optimiseWith (Optimise isFree) ps = go [] (mkPath ps)
    where
    go acc wholePath = do

        mapM_ print $ removal wholePath

