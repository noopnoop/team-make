{-# LANGUAGE TupleSections #-}
module Calc
  ( makeList
  , accountFilter
  ) where

import           Data.List.Extra                ( disjoint )
import qualified Data.Set                      as Set
import           Types                          ( Account
                                                , Character
                                                , DB
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

accountFilter :: Account -> DB Team -> DB Team
accountFilter acc = makeDB . filter (flip ownedTeam acc . fst) . getDB

-- | Checks if two teams don't overlap- that is, there's no character who's on both teams.
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team HuTao Albedo Zhongli Xingqiu) == False
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team Ganyu Xiangling Bennett Zhongli) == True
doesntOverlap :: Team -> Team -> Bool
doesntOverlap (Team a b c d) (Team w x y z) =
  disjoint [a, b, c, d] [w, x, y, z]

owned :: Character -> Account -> Bool
owned = Set.member

ownedTeam :: Team -> Account -> Bool
ownedTeam tm acc = all (`owned` acc) $ asList tm

asList :: Team -> [Character]
asList (Team a b c d) = [a, b, c, d]
