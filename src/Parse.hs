{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- Parse.hs
-- Various parsers used in the app
module Parse where
import           Control.Applicative            ( Alternative((<|>))
                                                , empty
                                                )
import           Control.Lens                   ( (#) )
import           Control.Monad.Except           ( MonadError
                                                , liftEither
                                                )
import           Data.Bifunctor                 ( bimap )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Char                      ( toLower )
import           Data.Csv                       ( Field
                                                , FromField(parseField)
                                                , FromRecord
                                                , HasHeader(NoHeader)
                                                , ToField(toField)
                                                , ToRecord(toRecord)
                                                , decode
                                                )
import qualified Data.Map                      as Map
import           Data.String                    ( IsString(fromString) )
import           Data.Text                      ( Text )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           GHC.Generics                   ( Generic )
import           Text.Read                      ( readMaybe )
import           Types                          ( Account
                                                , AsStartupError(_ParseError)
                                                , Character(..)
                                                , DB
                                                , InvestmentLevel
                                                , Team(Team)
                                                , makeDB
                                                )
import           Util                           ( capitalize
                                                , removeBadChars
                                                )

newtype DBFloat = DBFloat {
  getFloat :: Float
} deriving (Eq, Show)

-- >>> import Data.Csv
-- >>> import Data.Vector
-- >>> removeBadChars $ show "\"12,345\""
-- >>> decode NoHeader "\"12,345\"" :: Either String (Vector (Only DBFloat))
-- "12345"
-- Right [Only {fromOnly = DBFloat {getFloat = 12345.0}}]
instance FromField DBFloat where
  parseField s =
    maybe empty pure $ DBFloat <$> (readMaybe $ removeBadChars $ show s)

data DBRow = DBRow Character Character Character Character DBFloat
  deriving (Show, Generic)

instance FromRecord DBRow

getRow :: DBRow -> (Team, Float)
getRow (DBRow a b c d n) = (Team a b c d, getFloat n)

instance FromField InvestmentLevel where
  parseField s = maybe empty pure $ readMaybe (removeBadChars $ show s)

data AccountRow = AccountRow Character InvestmentLevel
  deriving (Show, Generic)

instance FromRecord AccountRow

getAccount :: AccountRow -> (Character, InvestmentLevel)
getAccount (AccountRow chr lvl) = (chr, lvl)

newtype Result = Result
  {
  getResult :: ((Team,Team), Float)
  } deriving (Show)

instance ToRecord Result where
  toRecord (Result ((Team a b c d, Team w x y z), n)) =
    toRecord (a, b, c, d, " | " :: String, w, x, y, z, " : " :: String, n) :: Vector
        Field


-- >>> import Data.Csv
-- >>> import Data.Vector
-- >>> decode NoHeader "Beidou,Fischl,Sucrose,Xingqiu,\"12,34\"\r\nBennett,Kazuha,Yoimiya,Yun Jin,\"34,414\"" :: Either String (Vector DBRow)
-- Right [DBRow Beidou Fischl Sucrose Xingqiu (DBFloat {getFloat = 1234.0}),DBRow Bennett KaedeharaKazuha Yoimiya YunJin (DBFloat {getFloat = 34414.0})]
instance FromField Character where
  parseField s =
    maybe empty pure
      $   (readMaybe $ capitalize $ removeBadChars $ show s)
      <|> (characterException $ toLower <$> (removeBadChars $ show s))

parseCharacter :: Text -> Maybe Character
parseCharacter s =
  (readMaybe $ capitalize $ removeBadChars $ show s)
    <|> (characterException $ toLower <$> (removeBadChars $ show s))

instance ToField Character where
  toField = fromString . show

-- >>> import Data.Csv
-- >>> import Data.Vector
-- >>> decode NoHeader "Raiden" :: Either String (Vector (Only Character))
-- Right [Only {fromOnly = RaidenShogun}]
characterException :: String -> Maybe Character
characterException "kazuha"  = Just KaedeharaKazuha
characterException "raiden"  = Just RaidenShogun
characterException "sara"    = Just KujouSara
characterException "kokomi"  = Just SangonomiyaKokomi
characterException "itto"    = Just AratakiItto
characterException "ayaka"   = Just KamisatoAyaka
characterException "ayato"   = Just KamisatoAyato
characterException "yae"     = Just YaeMiko
characterException "yaemiko" = Just YaeMiko
characterException "childe"  = Just Tartaglia
characterException "yunjin"  = Just YunJin
characterException "hutao"   = Just HuTao
characterException "travelergeo" = Just Traveler
characterException "traveleranemo" = Just Traveler
characterException "travelerelectro" = Just Traveler
characterException _         = Nothing

parseFile
  :: (FromRecord a, AsStartupError e, MonadError e m)
  => (Vector a -> b)
  -> ByteString
  -> m b
parseFile fn = liftEither . bimap (_ParseError #) fn . decode NoHeader

parseData :: (AsStartupError e, MonadError e m) => ByteString -> m (DB Team)
parseData = parseFile $ makeDB . map getRow . V.toList

parseAccount :: (AsStartupError e, MonadError e m) => ByteString -> m Account
parseAccount = parseFile $ Map.fromList . map getAccount . V.toList
