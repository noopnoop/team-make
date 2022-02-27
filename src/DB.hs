module DB
  ( keys
  , getPairDamage
  ) where
import           Control.Lens                   ( (#) )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError
                                                , liftEither
                                                )
import qualified Data.Map                      as Map
import           Types                          ( AsMathError(_TeamNotFound)
                                                , DB
                                                , MathError(TeamNotFound)
                                                , getDB
                                                )
import           Util                           ( maybeToEither )

-- | Returns a list of all keys in a database.
keys :: DB a -> [a]
keys = map fst . getDB

-- | Returns a list of all values in a database.
vals :: DB a -> [Float]
vals = map snd . getDB

-- | Returns the value of a key if that key is in a database, otherwise returns a "TeamNotFound" error.
lookupDB
  :: (Show a, Ord a, AsMathError e, MonadError e m) => a -> DB a -> m Float
lookupDB x db =
  liftEither
    $ maybeToEither (_TeamNotFound # show x)
    $ Map.lookup x
    $ Map.fromList
    $ getDB db

-- | Given a pair of keys and a database, returns the sum of their values. Can fail, e.g. if a key isn't in the database.
getPairDamage
  :: (Show a, Ord a, AsMathError e, MonadError e m) => (a, a) -> DB a -> m Float
getPairDamage (x, y) db = liftM2 (+) (lookupDB x db) (lookupDB y db)
