{-# LANGUAGE TupleSections #-}
module Calc
  ( makeList
  ) where

import           Data.List.Extra                ( disjoint )
import           Types                          ( DB
                                                , Team(Team)
                                                , getDB
                                                , makeDB
                                                )

makeList :: DB Team -> DB (Team, Team)
makeList = makeDB . map reArrange . filter runFilter . pairs . getDB
 where
  runFilter ((t1, _), (t2, _)) = doesntOverlap t1 t2
  reArrange ((t1, x1), (t2, x2)) = ((t1, t2), x1 + x2)

pairs :: [a] -> [(a, a)]
pairs []       = []
pairs (x : xs) = map (x, ) xs <> pairs xs

-- | Checks if two teams don't overlap- that is, there's no character who's on both teams.
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team HuTao Albedo Zhongli Xingqiu) == False
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team Ganyu Xiangling Bennett Zhongli) == True
doesntOverlap :: Team -> Team -> Bool
doesntOverlap (Team a b c d) (Team w x y z) =
  disjoint [a, b, c, d] [w, x, y, z]
