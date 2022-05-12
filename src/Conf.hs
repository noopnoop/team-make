{-# LANGUAGE TemplateHaskell #-}
-- Conf.hs
-- Contains the types Conf and PartialConf used to configure the app,
-- as well as some utility functions going between them.
module Conf
  ( Conf(..)
  , defaultConf
  , HasConf(..)
  , PartialConf(..)
  , mkConf
  ) where
import           Control.Lens                   ( (#)
                                                , makeClassy
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Types                          ( AsStartupError
                                                , _ConfError
                                                )

data Conf = Conf
  { _databasePath     :: FilePath
  , _accountPath      :: FilePath
  , _outputPath       :: FilePath
  , _useAccount       :: Bool
  , _useModifications :: Bool
  , _jsonDatabase     :: Bool
  }
  deriving (Eq, Show)

data PartialConf = PartialConf
  { _maybeDatabasePath     :: Last FilePath
  , _maybeAccountPath      :: Last FilePath
  , _maybeOutputPath       :: Last FilePath
  , _maybeUseAccount       :: Last Bool
  , _maybeUseModifications :: Last Bool
  , _maybejsonDatabase     :: Last Bool
  }
  deriving (Eq, Show)

newtype Last a = Last { getLast :: Maybe a }
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) x (Last Nothing ) = x
  (<>) _ (Last (Just x)) = Last (Just x)

instance Monoid (Last a) where
  mempty = Last Nothing

instance Semigroup PartialConf where
  (<>) (PartialConf a b c d e f) (PartialConf w x y z v u) =
    PartialConf (a <> w) (b <> x) (c <> y) (d <> z) (e <> v) (f <> u)

instance Monoid PartialConf where
  mempty = PartialConf mempty mempty mempty mempty mempty mempty

mkLast :: a -> Last a
mkLast = Last . Just

mkPartial :: Conf -> PartialConf
mkPartial (Conf a b c d e f) =
  PartialConf (mkLast a) (mkLast b) (mkLast c) (mkLast d) (mkLast e) (mkLast f)

defaultConf :: PartialConf
defaultConf = mkPartial $ Conf "./data/data.json"
                               "./data/myaccount.csv"
                               "./data/output.csv"
                               True
                               True
                               True

mkConf :: (AsStartupError e, MonadError e m) => PartialConf -> m Conf
mkConf (PartialConf (Last (Just a)) (Last (Just b)) (Last (Just c)) (Last (Just d)) (Last (Just e)) (Last (Just f)))
  = pure $ Conf a b c d e f
mkConf _ = throwError (_ConfError # ())

$(makeClassy ''Conf)
