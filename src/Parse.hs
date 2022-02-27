{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where
import           Control.Applicative            ( Alternative((<|>))
                                                , empty
                                                )
import           Data.Csv                       ( FromField(parseField)
                                                , FromRecord
                                                , ToField(toField)
                                                , ToRecord(toRecord), Field
                                                )
import           Data.String                    ( IsString(fromString) )
import           GHC.Generics                   ( Generic )
import           Text.Read                      ( readMaybe )
import           Types                          ( Character(..)
                                                , Team(Team)
                                                )
import           Util                           ( removeBadChars )
import Data.Vector (Vector)

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

newtype Result = Result
  {
  getResult :: ((Team,Team), Float)
  } deriving (Show)

instance ToRecord Result where
  toRecord (Result ((Team a b c d,Team w x y z),n)) =
    toRecord (a,b,c,d, " | " :: String , w,x,y,z, " : " :: String, n) :: Vector Field


-- >>> import Data.Csv
-- >>> import Data.Vector
-- >>> decode NoHeader "Beidou,Fischl,Sucrose,Xingqiu,\"12,34\"\r\nBennett,Kazuha,Yoimiya,Yun Jin,\"34,414\"" :: Either String (Vector DBRow)
-- Right [DBRow Beidou Fischl Sucrose Xingqiu (DBFloat {getFloat = 1234.0}),DBRow Bennett KaedeharaKazuha Yoimiya YunJin (DBFloat {getFloat = 34414.0})]
instance FromField Character where
  parseField s =
    maybe empty pure
      $   (readMaybe $ removeBadChars $ show s)
      <|> (characterException $ removeBadChars $ show s)

instance ToField Character where
  toField = fromString . show

-- >>> import Data.Csv
-- >>> import Data.Vector
-- >>> decode NoHeader "Raiden" :: Either String (Vector (Only Character))
-- Right [Only {fromOnly = RaidenShogun}]
characterException :: String -> Maybe Character
characterException "Kazuha" = Just KaedeharaKazuha
characterException "Raiden" = Just RaidenShogun
characterException "Sara"   = Just KujouSara
characterException "Kokomi" = Just SangonomiyaKokomi
characterException "Itto"   = Just AratakiItto
characterException "Ayaka"  = Just KamisatoAyaka
characterException "Yae"    = Just YaeMiko
characterException "Childe" = Just Tartaglia
characterException _        = Nothing
