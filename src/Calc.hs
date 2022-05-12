{-# LANGUAGE TupleSections #-}
-- Calc.hs
-- Pure functions that run calculations on a database of teams.
module Calc
  ( makeList
  , accountFilter
  , applyMods
  , bestResin
  , bestPrimo
  ) where

import           Data.List.Extra                ( disjoint )
import qualified Data.Map                      as Map
import           Debug.Trace                    ( traceShowId
                                                )
import           Extra                          ( intToFloat )
import           Types                          ( Account
                                                , Character(..)
                                                , DB
                                                , DmgProfile(..)
                                                , InvestmentLevel(..)
                                                , Team(Team)
                                                , allCharacters
                                                , avgRollsToGet
                                                , charDmgType
                                                , getDB
                                                , makeDB
                                                , teamToList
                                                )

--pass in a modded team db
makeList :: DB Team -> DB (Team, Team)
makeList = makeDB . map reArrange . filter runFilter . pairs . getDB
 where
  runFilter ((t1, _), (t2, _)) = doesntOverlap t1 t2
  reArrange ((t1, x1), (t2, x2)) = ((t1, t2), x1 + x2)

pairs :: [a] -> [(a, a)]
pairs []       = []
pairs (x : xs) = map (x, ) xs <> pairs xs

promote :: Character -> Account -> Account
promote chr acc = case Map.lookup chr acc of
  Nothing -> acc
  Just il -> case il of
    None      -> Map.insert chr BareBones acc
    BareBones -> Map.insert chr Moderate acc
    Moderate  -> Map.insert chr Invested acc
    Invested  -> Map.insert chr Invested acc
    HyperInvested -> acc

-- pass in a modded team db
bestResin :: Account -> DB Team -> DB Character
bestResin acc db = makeDB $ doRep db <$> Map.toList acc
 where
  doRep db' (chr, _) =
    (chr, snd $ head $ getDB $ makeList $ applyMods (promote chr acc) db')

-- pass in an unmodded team db
bestPrimo :: Account -> DB Team -> DB Character
bestPrimo acc db = makeDB $ doRep <$> allCharacters
 where
  currentBest =
    snd $ head $ getDB $ makeList $ applyMods acc $ accountFilter acc db
  doRep chr =
    (chr, (newBest chr - currentBest) / intToFloat (avgRollsToGet chr))
  newBest chr =
    snd
      $ head
      $ getDB
      $ makeList
      $ applyMods (Map.insert chr HyperInvested acc)
      $ accountFilter (Map.insert chr HyperInvested acc) db

accountFilter :: Account -> DB Team -> DB Team
accountFilter acc = makeDB . filter (flip ownedTeam acc . fst) . getDB


-- | Checks if two teams don't overlap- that is, there's no character who's on both teams.
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team HuTao Albedo Zhongli Xingqiu) == False
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team Ganyu Xiangling Bennett Zhongli) == True
doesntOverlap :: Team -> Team -> Bool
doesntOverlap (Team a b c d) (Team w x y z) =
  disjoint [a, b, c, d] [w, x, y, z]

dpsPerRoll :: (Team, Float) -> (Team, Float)
dpsPerRoll (team, dps) =
  (team, dps / intToFloat (sum $ avgRollsToGet <$> teamToList team))

owned :: Character -> Account -> Bool
owned chr acc = case Map.lookup chr acc of
  Nothing -> False
  Just il -> case il of
    None -> False
    _    -> True

ownedTeam :: Team -> Account -> Bool
ownedTeam tm acc = all (`owned` acc) $ asList tm

onTeam :: Character -> Team -> Bool
onTeam char team = char `elem` teamToList team

addDmgConditional :: Float -> (a -> Bool) -> (a, Float) -> (a, Float)
addDmgConditional dmg cond (a, base) =
  if cond a then (a, base + dmg) else (a, base)

modifications :: Account -> [(Team, Float) -> (Team, Float)]
modifications acc = --[] --[dpsPerRoll]
  [ addDmgConditional 7500    (RaidenShogun `onTeam`)
  , addDmgConditional 6000    (HuTao `onTeam`)
  , addDmgConditional (-2000) (Ningguang `onTeam`)
  , modifyByInvestment acc
  ]

-- kinda cursed
applyMods :: Account -> DB Team -> DB Team
applyMods acc db = makeDB $ flip (foldr ($)) (modifications acc) <$> getDB db

asList :: Team -> [Character]
asList (Team a b c d) = [a, b, c, d]

modifyByInvestment :: Account -> (Team, Float) -> (Team, Float)
modifyByInvestment acc (Team a b c d, dps) =
  ( Team a b c d
  , dps
    * getCharMultiplier a acc
    * getCharMultiplier b acc
    * getCharMultiplier c acc
    * getCharMultiplier d acc
  )

getCharMultiplier :: Character -> Account -> Float
getCharMultiplier chr acc = case charDmgType chr of
  MainDps -> 0.5 * (1 + investmentMultiplier)
  SubDps  -> 0.25 * (3 + investmentMultiplier)
  Support -> 1
 where
  investmentMultiplier = case Map.lookup chr acc of
    Nothing -> 0
    Just il -> case il of
      None      -> 0
      BareBones -> 0
      Moderate -> 0.33
      Invested  -> 0.75
      HyperInvested  -> 1
