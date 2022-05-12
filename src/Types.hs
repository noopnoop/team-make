{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
-- Types.hs
-- Most of the types used in the app.
module Types where
import           Control.Exception              ( SomeException )
import           Control.Lens                   ( makeClassyPrisms )
import           Data.Map                       ( Map )
import           GHC.Exts                       ( sortWith )

data Character = Albedo
  | Aloy
  | Amber
  | AratakiItto
  | Barbara
  | Beidou
  | Bennett
  | Chongyun
  | Diluc
  | Diona
  | Eula
  | Fischl
  | Ganyu
  | Gorou
  | HuTao
  | Jean
  | KaedeharaKazuha
  | Kaeya
  | KamisatoAyaka
  | KamisatoAyato
  | Keqing
  | Klee
  | KujouSara
  | Lisa
  | Mona
  | Ningguang
  | Noelle
  | Paimon
  | Qiqi
  | RaidenShogun
  | Razor
  | Rosaria
  | SangonomiyaKokomi
  | Sayu
  | Shenhe
  | Sucrose
  | Tartaglia
  | Thoma
  | Traveler
  | Venti
  | Xiangling
  | Xiao
  | Xingqiu
  | Xinyan
  | YaeMiko
  | Yanfei
  | Yoimiya
  | YunJin
  | Zhongli
  deriving (Eq, Ord, Show, Read)

allCharacters :: [Character]
allCharacters =
  [ Albedo
  , Aloy
  , Amber
  , AratakiItto
  , Barbara
  , Beidou
  , Bennett
  , Chongyun
  , Diluc
  , Diona
  , Eula
  , Fischl
  , Ganyu
  , Gorou
  , HuTao
  , Jean
  , KaedeharaKazuha
  , Kaeya
  , KamisatoAyaka
  , Keqing
  , Klee
  , KujouSara
  , Lisa
  , Mona
  , Ningguang
  , Noelle
  , Qiqi
  , RaidenShogun
  , Razor
  , Rosaria
  , SangonomiyaKokomi
  , Sayu
  , Shenhe
  , Sucrose
  , Tartaglia
  , Thoma
  , Traveler
  , Venti
  , Xiangling
  , Xiao
  , Xingqiu
  , Xinyan
  , YaeMiko
  , Yanfei
  , Yoimiya
  , YunJin
  , Zhongli
  ]

data Team = Team Character Character Character Character
  deriving (Eq, Ord, Show)

teamToList :: Team -> [Character]
teamToList (Team a b c d) = [a, b, c, d]

listToTeam :: [Character] -> Team
listToTeam [a, b, c, d] = Team a b c d
listToTeam [a, b , c] = Team a b c Paimon
listToTeam [a,b] = Team a b Paimon Paimon
listToTeam [a] = Team a Paimon Paimon Paimon
listToTeam _            = error "awakfbioa"

newtype DB a = DB [(a, Float)]
  deriving (Eq, Show)

getDB :: DB a -> [(a, Float)]
getDB (DB x) = x

makeDB :: [(a, Float)] -> DB a
makeDB = DB . reverse . sortWith snd

type Account = Map Character InvestmentLevel

data InvestmentLevel =
    None      -- don't use this character
  | BareBones -- use but assume 0 dps
  | Moderate  -- assume level 60/70, level 6 talents, main stats only.
  | Invested  -- assume level 6/8 talents, 150 cv, 70/80, about 70% dmg from kqm standard
  | HyperInvested  -- assume kqm standatd investment
  deriving (Eq, Show, Read)

data DmgProfile =
    MainDps
  | SubDps
  | Support
  deriving (Eq,Show)

charDmgType :: Character -> DmgProfile
charDmgType x = case x of
  Albedo            -> SubDps
  Aloy              -> Support
  Amber             -> Support
  AratakiItto       -> MainDps
  Barbara           -> Support
  Beidou            -> MainDps
  Bennett           -> Support
  Chongyun          -> Support
  Diluc             -> MainDps
  Diona             -> Support
  Eula              -> MainDps
  Fischl            -> SubDps
  Ganyu             -> MainDps
  Gorou             -> Support
  HuTao             -> MainDps
  Jean              -> Support
  KaedeharaKazuha   -> SubDps
  Kaeya             -> SubDps
  KamisatoAyaka     -> MainDps
  KamisatoAyato     -> MainDps
  Keqing            -> MainDps
  Klee              -> MainDps
  KujouSara         -> SubDps
  Lisa              -> Support
  Mona              -> Support
  Ningguang         -> MainDps
  Noelle            -> MainDps
  Paimon            -> Support
  Qiqi              -> Support
  RaidenShogun      -> MainDps
  Razor             -> MainDps
  Rosaria           -> SubDps
  SangonomiyaKokomi -> Support
  Sayu              -> Support
  Shenhe            -> SubDps
  Sucrose           -> Support
  Tartaglia         -> MainDps
  Thoma             -> Support
  Traveler          -> Support
  Venti             -> SubDps
  Xiangling         -> MainDps
  Xiao              -> MainDps
  Xingqiu           -> SubDps
  Xinyan            -> Support
  YaeMiko           -> SubDps
  Yanfei            -> MainDps
  Yoimiya           -> MainDps
  YunJin            -> SubDps
  Zhongli           -> Support

avgRollsToGet :: Character -> Int
avgRollsToGet chr = case chr of
  Albedo            -> 94
  Aloy              -> 0
  Amber             -> 0
  AratakiItto       -> 94
  Barbara           -> 0
  Beidou            -> 36
  Bennett           -> 36
  Chongyun          -> 36
  Diluc             -> 624
  Diona             -> 36
  Eula              -> 94
  Fischl            -> 36
  Ganyu             -> 94
  Gorou             -> 36
  HuTao             -> 94
  Jean              -> 624
  KaedeharaKazuha   -> 94
  Kaeya             -> 0
  KamisatoAyaka     -> 94
  KamisatoAyato     -> 94
  Keqing            -> 624
  Klee              -> 94
  KujouSara         -> 36
  Lisa              -> 0
  Mona              -> 624
  Ningguang         -> 36
  Noelle            -> 10
  Paimon            -> 0
  Qiqi              -> 624
  RaidenShogun      -> 94
  Razor             -> 36
  Rosaria           -> 36
  SangonomiyaKokomi -> 94
  Sayu              -> 36
  Shenhe            -> 94
  Sucrose           -> 36
  Tartaglia         -> 94
  Thoma             -> 36
  Traveler          -> 0
  Venti             -> 94
  Xiangling         -> 0
  Xiao              -> 94
  Xingqiu           -> 36
  Xinyan            -> 36
  YaeMiko           -> 94
  Yanfei            -> 36
  Yoimiya           -> 94
  YunJin            -> 36
  Zhongli           -> 94

data AppError =
    AppStartupError StartupError
  | AppPrintError PrintError
  deriving (Show)

data StartupError = ReadError SomeException
  | ParseError String
  | ConfError
  deriving (Show)

data PrintError = WriteError SomeException
  deriving Show

$(makeClassyPrisms ''StartupError)
$(makeClassyPrisms ''PrintError)
$(makeClassyPrisms ''AppError)

instance AsStartupError AppError where
  _StartupError = _AppStartupError . _StartupError

instance AsPrintError AppError where
  _PrintError = _AppPrintError . _PrintError
