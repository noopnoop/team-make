{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Types
  ( Character(..)
  , Team(..)
  , MathError(..)
  , StartupError(..)
  , PrintError(..)
  , AppError(..)
  , DB
  , getDB
  , makeDB
  , AsAppError(..)
  , AsMathError(..)
  , AsStartupError(..)
  , AsPrintError(..)
  ) where
import           Control.Exception              ( SomeException )
import           Control.Lens                   ( makeClassyPrisms )
import           GHC.Exts                       ( sortWith )
import GHC.Generics (Generic)

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
  | Keqing
  | Klee
  | KujouSara
  | Lisa
  | Mona
  | Ningguang
  | Noelle
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

data Team = Team Character Character Character Character
  deriving (Eq, Ord, Show)

newtype DB a = DB [(a, Float)]
  deriving (Eq, Show)

getDB :: DB a -> [(a, Float)]
getDB (DB x) = x

makeDB :: [(a, Float)] -> DB a
makeDB = DB . reverse . sortWith snd

data AppError = AppMathError MathError
  | AppStartupError StartupError
  | AppPrintError PrintError
  deriving (Show)

data MathError = NoAvailablePartner
  | TeamNotFound String
  deriving (Show)

data StartupError = FileError SomeException
  | ParseError String
  | NoFileGiven
  deriving (Show)

data PrintError = WriteError SomeException
  deriving (Show)

$(makeClassyPrisms ''MathError)
$(makeClassyPrisms ''StartupError)
$(makeClassyPrisms ''PrintError)
$(makeClassyPrisms ''AppError)

instance AsMathError AppError where
  _MathError = _AppMathError . _MathError

instance AsStartupError AppError where
  _StartupError = _AppStartupError . _StartupError

instance AsPrintError AppError where
  _PrintError = _AppPrintError . _PrintError