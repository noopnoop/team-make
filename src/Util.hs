module Util where
-- Util.hs
-- Miscellaneous helpful functions.
import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Monad.Except           ( MonadError
                                                , liftEither
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Bifunctor                 ( Bifunctor(first) )
import Data.Char (toUpper)

removeBadChars :: String -> String
removeBadChars = filter (`notElem` badChar)
  where badChar = [' ', '\"', '\n', '\r', '\\', ',']

safeHead :: e -> [a] -> Either e a
safeHead err []      = Left err
safeHead _   (x : _) = Right x

appTry :: (MonadIO m, MonadError e m) => (SomeException -> e) -> IO a -> m a
appTry fn ia = liftEither =<< liftIO (first fn <$> try ia)

mte :: e -> Maybe a -> Either e a
mte e Nothing  = Left e
mte _ (Just x) = Right x

-- UFCK!!!!
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs