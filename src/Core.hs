module Core where
import           Calc                           ( accountFilter
                                                , makeList
                                                )
import           Conf                           ( Conf(..)
                                                , HasConf(..)
                                                , defaultConf
                                                )
import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Lens                   ( (#) )
import           Control.Lens.Getter            ( (^.) )
import           Control.Monad.Error.Class      ( liftEither )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(catchError)
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , ReaderT
                                                , asks
                                                )
import           Data.Bifunctor                 ( Bifunctor(bimap, first) )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Csv                       ( HasHeader(NoHeader)
                                                , decode
                                                , encode, FromRecord, Only (fromOnly)
                                                )
import qualified Data.Vector                   as V
import           Parse                          ( Result(Result)
                                                , getRow
                                                )
import           Types                          ( AppError
                                                , AsAppError
                                                , AsPrintError(_WriteError)
                                                , AsStartupError
                                                  ( _ParseError
                                                  , _ReadError
                                                  )
                                                , DB
                                                , Team
                                                , getDB
                                                , makeDB, Account
                                                )
import Data.Vector (Vector)
import qualified Data.Set as Set

type App = ReaderT Conf (ExceptT AppError IO) ()

app :: App
app = flip catchError (liftIO . print) $ do
  (db', acct') <- readData
  out          <- asks _outputPath
  db           <- parseData db'
  printResults out $ makeList db

-- startApp :: (MonadError e m, MonadIO m, AsStartupError e) => m Conf
-- startApp = do
--   --args <- liftIO getArgs
--   --liftEither $ safeHead (_NoFileGiven # ()) args
--   return defaultConf

getConf :: Monad m => m Conf
getConf = return defaultConf

readData
  :: (MonadIO m, AsStartupError e, MonadError e m, HasConf r, MonadReader r m)
  => m (ByteString, ByteString)
readData = do
  env  <- ask
  db   <- appTry (_ReadError #) (BS.readFile $ env ^. databasePath)
  acct <- appTry (_ReadError #) (BS.readFile $ env ^. accountPath)
  return (db, acct)

appTry
  :: (MonadIO m, MonadError e m)
  => (SomeException -> e)
  -> IO a
  -> m a
appTry fn ma = liftEither =<< liftIO (first fn <$> try ma)

parseFile :: (FromRecord a, AsStartupError e, MonadError e m) => (Vector a -> b) -> ByteString -> m b
parseFile fn =
  liftEither
    . bimap (_ParseError #) fn
    . decode NoHeader

parseData :: (AsStartupError e, MonadError e m) => ByteString -> m (DB Team)
parseData = parseFile $ makeDB . map getRow . V.toList

parseAccount :: (AsStartupError e, MonadError e m) => ByteString -> m Account
parseAccount = parseFile $ Set.fromList . map fromOnly . V.toList 

printResults
  :: (MonadIO m, AsPrintError e, MonadError e m)
  => FilePath
  -> DB (Team, Team)
  -> m ()
printResults fp db = appTry (_WriteError #) (BS.writeFile fp $ encode $ Result <$> getDB db)
