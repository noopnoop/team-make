{-# LANGUAGE TupleSections #-}
module Calc where

import           Control.Lens                   ( (#)
                                                , _Left
                                                , _Right
                                                )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError
                                                , liftEither
                                                )
import           DB                             ( getPairDamage
                                                , keys
                                                )
import           Data.Either                    ( isRight )
import           Data.List.Extra                ( disjoint )
import           GHC.Exts                       ( sortWith )
import           Types                          ( AsMathError
                                                  ( _MathError
                                                  , _NoAvailablePartner
                                                  )
                                                , Character(..)
                                                , DB
                                                , MathError(..)
                                                , Team(Team)
                                                , makeDB
                                                )
import Util (dropUntil)

-- | Given a database of teams, find the pair with the most DPS. The heart of the program.
optimize :: (AsMathError e, MonadError e m) => DB Team -> m (DB (Team, Team))
optimize db = case filter isRight $ map (`findPartner` db) (keys db) of
  []  -> liftEither $ _Left . _NoAvailablePartner # ()
  rxs -> liftEither $ makeDB <$> do
    xs <- sequence rxs
    let pairs = zip (keys db) xs
    traverse (\x -> fmap (x, ) (getPairDamage x db)) pairs

-- | Given a team, finds the next best possible team that doesn't overlap with it.
-- >>> import Data.Either ( isLeft )
-- >>> findPartner (Team HuTao Amber Sucrose Xingqiu) [Team Ganyu Xiangling Bennett Zhongli] == Right (Team Ganyu Xiangling Bennett Zhongli)
-- >>> isLeft (findPartner (Team HuTao Amber Sucrose Xingqiu) [Team HuTao Albedo Zhongli Xingqiu] ) == True
findPartner :: (AsMathError e, MonadError e m) => Team -> DB Team -> m Team
findPartner tm rkg = case filter (doesntOverlap tm) $ dropUntil (==tm) (keys rkg) of
  []    -> liftEither $ _Left . _NoAvailablePartner # ()
  x : _ -> liftEither $ Right x

-- | Checks if two teams don't overlap- that is, there's no character who's on both teams.
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team HuTao Albedo Zhongli Xingqiu) == False
-- >>> doesntOverlap (Team HuTao Amber Sucrose Xingqiu) (Team Ganyu Xiangling Bennett Zhongli) == True
doesntOverlap :: Team -> Team -> Bool
doesntOverlap (Team a b c d) (Team w x y z) =
  disjoint [a, b, c, d] [w, x, y, z]
