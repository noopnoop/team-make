{-# LANGUAGE DeriveGeneric #-}
module Json where

import Data.Aeson

import GHC.Generics
import Data.Text (Text)
import Types (Team, Character, listToTeam, DB, makeDB, AsStartupError(..))
import Parse (parseCharacter)
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Error (MonadError)
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Lens.Operators ((#))
import Util (mte)

data JsonTeamMeta = JsonTeamMeta
  { author :: Maybe Text
  , config :: Maybe Text
  , description :: Maybe Text
  , hash :: Maybe Text
  , team :: [JsonCharacter]
  , dps :: Float
  , mode :: Maybe Text
  , duration :: Maybe Float
  , target_count :: Maybe Int
  , viewer_key :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON JsonTeamMeta

-- >>> import qualified Data.ByteString.Lazy as BS
-- >>> decode <$> (BS.readFile "data/test.json") :: IO (Maybe [JsonTeamMeta])
-- Just [JsonTeamMeta {author = Just "Charliex3000#9403", config = Just "options swap_delay=12 debug=true iteration=1000 duration=102 workers=30 mode=sl;\n\n\n\n\n\nayaka char lvl=90/90 cons=0 talent=9,9,9; \nayaka add weapon=\"amenomakageuchi\" refine=5 lvl=90/90;\nayaka add set=\"blizzardstrayer\" count=5;\nayaka add stats hp=4780 atk=311 atk%=0.466 cryo%=0.466 cd=0.622 ; #main\nayaka add stats def%=0.124 def=39.36 hp=507.88 hp%=0.0992 atk=33.08 atk%=0.0992 er=0.2204 em=39.64 cr=0.3972 cd=0.662;\n\n\ndiona char lvl=90/90 cons=6 talent=9,9,9; \ndiona add weapon=\"favoniuswarbow\" refine=3 lvl=90/90;\ndiona add set=\"noblesseoblige\" count=4;\ndiona add set=\"paleflame\" count=1;\ndiona add stats hp=4780 atk=311 hp%=1.398 ; #main\ndiona add stats def%=0.124 def=39.36 hp=507.88 hp%=0.0992 atk=33.08 atk%=0.0992 er=0.4408 em=39.64 cr=0.3972 cd=0.3972;\n\n\nkaedeharakazuha char lvl=90/90 cons=0 talent=9,9,9; \nkaedeharakazuha add weapon=\"favoniussword\" refine=3 lvl=90/90;\nkaedeharakazuha add set=\"viridescentvenerer\" count=5;\nkaedeharakazuha add stats hp=4780 atk=311 em=561 ; #main\nkaedeharakazuha add stats def%=0.124 def=39.36 hp=507.88 hp%=0.0992 atk=33.08 atk%=0.1984 er=0.3306 em=118.92 cr=0.3972 cd=0.1324;\n\nmona char lvl=90/90 cons=0 talent=9,9,9; \nmona add weapon=\"thrillingtalesofdragonslayers\" refine=5 lvl=90/90;\nmona add set=\"tenacityofthemillelith\" count=4;\nmona add stats hp=4780 atk=311 er=0.518 hydro%=0.466 cr=0.311 ; #main\nmona add stats def%=0.124 def=39.36 hp=507.88 hp%=0.0992 atk=33.08 atk%=0.5952 er=0.6612 em=39.64 cr=0.0662 cd=0.1324;\n\nactive diona;\ntarget lvl=100 pyro=0.1 dendro=0.1 hydro=0.1 electro=0.1 geo=0.1 anemo=0.1 physical=.1 cryo=.1;\nenergy every interval=480,720 amount=1;\n\ndiona burst;\nkazuha skill,high_plunge,burst;\nmona burst,skill;\nayaka dash,attack,skill,burst;\nkazuha skill,high_plunge;\nmona attack:3;\ndiona skill[hold=1];\nayaka dash,attack:2,charge,attack:2,charge,skill,dash,attack:2,charge;\nrestart;\n", description = Just "Ayaka Freeze.", hash = Just "932ea0bee663e52ba8f12907d6b49bee901912b2", team = [JsonCharacter {name = "ayaka", con = Just 0, weapon = Just "amenomakageuchi", refine = Just 5, er = Just 0.2204, talents = JsonTalents {attack = 9, skill = 9, burst = 9}},JsonCharacter {name = "diona", con = Just 6, weapon = Just "favoniuswarbow", refine = Just 3, er = Just 0.4408, talents = JsonTalents {attack = 9, skill = 9, burst = 9}},JsonCharacter {name = "yunjin", con = Just 0, weapon = Just "favoniussword", refine = Just 3, er = Just 0.3306, talents = JsonTalents {attack = 9, skill = 9, burst = 9}},JsonCharacter {name = "mona", con = Just 0, weapon = Just "thrillingtalesofdragonslayers", refine = Just 5, er = Just 1.1792, talents = JsonTalents {attack = 9, skill = 9, burst = 9}}], dps = 40840.07, mode = Just "sl", duration = Just 102.0, target_count = Just 1, viewer_key = Just "mU6Ssg7eVmDZVgTTwMZY_"}]

data JsonCharacter = JsonCharacter
  { name :: Text
  , con :: Maybe Int
  , weapon :: Maybe Text
  , refine :: Maybe Int
  , er :: Maybe Float
  , talents :: JsonTalents
  } deriving (Show, Generic)

instance FromJSON JsonCharacter

data JsonTalents = JsonTalents
  { attack :: Int
  , skill :: Int
  , burst :: Int
  } deriving (Show, Generic)

instance FromJSON JsonTalents

convertJsonTeamMeta :: JsonTeamMeta -> Maybe (Team, Float)
convertJsonTeamMeta jtm = do
  tm <- traverse convertJsonCharacter $ team jtm
  return (listToTeam tm , dps jtm)

convertJsonCharacter :: JsonCharacter -> Maybe Character
convertJsonCharacter = parseCharacter . name

parseJson :: (AsStartupError e, MonadError e m) => ByteString -> m (DB Team)
parseJson bs = liftEither $ mte (_ParseError # "json parse error") $ do
  tms <- decode bs
  makeDB <$> traverse convertJsonTeamMeta tms

