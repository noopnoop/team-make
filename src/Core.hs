module Core where
import           Calc                           ( makeList )
import           Control.Exception              ( try )
import           Control.Lens                   ( (#) )
import           Control.Monad.Error.Class      ( liftEither )
import           Control.Monad.Except           ( MonadError(catchError), ExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Bifunctor                 ( Bifunctor(bimap, first) )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Csv                       ( HasHeader(NoHeader)
                                                , decode, encode
                                                )
import qualified Data.Vector                   as V
import           Parse                          ( getRow, Result (Result) )
import           System.Environment             ( getArgs )
import           Types                          ( AppError
                                                
                                                , AsStartupError
                                                  ( _FileError
                                                  , _NoFileGiven
                                                  , _ParseError
                                                  )
                                                , DB
                                                
                                                , Team
                                                , makeDB, AsPrintError (_WriteError), getDB
                                                )
import           Util                           ( safeHead )


type App = ExceptT AppError IO ()

app :: App
app = catchError
  ((startApp >>= readData >>= parseData) >>= printResults "./data/output.csv" . makeList)
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