module Core where
import           Calc                           ( optimize )
import           Control.Exception              ( try )
import           Control.Exception.Base         ( SomeException )
import           Control.Lens                   ( (#) )
import           Control.Monad.Error.Class      ( liftEither )
import           Control.Monad.Except           ( ExceptT(ExceptT)
                                                , MonadError(catchError)
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Bifunctor                 ( Bifunctor(bimap, first) )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Csv                       ( FromRecord(parseRecord)
                                                , HasHeader(NoHeader)
                                                , decode, encode
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Parse                          ( getRow, Result (Result) )
import           System.Environment             ( getArgs )
import           Types                          ( AppError
                                                , AsAppError(..)
                                                , AsMathError
                                                , AsStartupError
                                                  ( _FileError
                                                  , _NoFileGiven
                                                  , _ParseError
                                                  )
                                                , DB
                                                , StartupError(FileError)
                                                , Team
                                                , makeDB, AsPrintError (_WriteError), getDB
                                                )
import           Util                           ( safeHead )


type App = ExceptT AppError IO ()

app :: App
app = catchError
  (startApp >>= readData >>= parseData >>= optimize >>= printResults "./data/output.csv")
  (liftIO . print)

startApp :: (MonadError e m, MonadIO m, AsStartupError e) => m FilePath
startApp = do
  args <- liftIO getArgs
  liftEither $ safeHead (_NoFileGiven # ()) args

readData
  :: (MonadIO m, AsStartupError e, MonadError e m) => FilePath -> m ByteString
readData fp =
  liftEither =<< liftIO (first (_FileError #) <$> try (BS.readFile fp))

parseData :: (AsStartupError e, MonadError e m) => ByteString -> m (DB Team)
parseData =
  liftEither
    . bimap (_ParseError #) (makeDB . map getRow . V.toList)
    . decode NoHeader

printResults :: (MonadIO m, AsPrintError e, MonadError e m) => FilePath -> DB (Team, Team) -> m ()
printResults fp db =
  liftEither =<< liftIO (first (_WriteError #) <$> try (BS.writeFile fp $ encode $ Result <$> getDB db))