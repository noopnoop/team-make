module Core where
import           Calc                           ( accountFilter
                                                , applyMods
                                                , bestPrimo
                                                , bestResin
                                                , makeList
                                                )
import           Conf                           ( Conf(..)
                                                , HasConf(..)
                                                , defaultConf
                                                , mkConf
                                                )
import           Control.Lens                   ( (#) )
import           Control.Lens.Getter            ( (^.) )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(catchError)
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , ReaderT
                                                , asks
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy.Char8    as BS
import           Json                           ( parseJson )
import           Parse                          ( parseAccount
                                                , parseData
                                                )
import           Types                          ( AppError
                                                , AsPrintError(_WriteError)
                                                , AsStartupError(_ReadError)
                                                , DB
                                                , getDB
                                                )
import           Util                           ( appTry )

app :: ReaderT Conf (ExceptT AppError IO) ()
app = flip catchError (liftIO . print) $ do
  (db', acct') <- readData
  out          <- asks _outputPath
  useAcct      <- asks _useAccount
  doMods       <- asks _useModifications
  jsonData     <- asks _jsonDatabase
  acct         <- parseAccount acct'
  alldb        <- if jsonData then parseJson db' else parseData db'
  let db     = if useAcct then accountFilter acct alldb else alldb
  let dbmods = if doMods then applyMods acct db else db
  -- you probably dont want to uncomment this next line but maybe someday itll be useful for something
  -- let alldbmods = if doMods then applyMods acct alldb else alldb 
  liftIO $ writeFile out ""
  printResults out $ makeList dbmods
  --printResults out $ bestResin acct dbmods
  printResults out $ bestPrimo acct alldb

-- startApp :: (MonadError e m, MonadIO m, AsStartupError e) => m Conf
-- startApp = do
--   --args <- liftIO getArgs
--   --liftEither $ safeHead (_NoFileGiven # ()) args
--   return defaultConf

getConf :: (MonadIO m, AsStartupError e, MonadError e m) => m Conf
getConf = mkConf defaultConf

getCmdConf :: (MonadIO m, AsStartupError e, MonadError e m) => m Conf
getCmdConf = undefined

readData
  :: (MonadIO m, AsStartupError e, MonadError e m, HasConf r, MonadReader r m)
  => m (ByteString, ByteString)
readData = do
  env  <- ask
  db   <- appTry (_ReadError #) (BS.readFile $ env ^. databasePath)
  acct <- appTry (_ReadError #) (BS.readFile $ env ^. accountPath)
  return (db, acct)

printResults
  :: (MonadIO m, AsPrintError e, MonadError e m, Show a)
  => FilePath
  -> DB a
  -> m ()
printResults fp db =
  appTry (_WriteError #) (appendFile fp $ unlines $ show <$> getDB db)
