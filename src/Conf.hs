{-# LANGUAGE TemplateHaskell #-}
module Conf
  ( Conf(..)
  , defaultAccount
  , defaultConf
  , HasConf(..)
  ) where
import           Control.Lens                   ( makeClassy )
import qualified Data.Set                      as Set
import           Types                          ( Account
                                                , Character(..)
                                                )

testAccount :: Account
testAccount = Set.fromList
  [ Albedo
  , Aloy
  , Amber
  , Barbara
  , Beidou
  , Bennett
  , Chongyun
  , Diluc
  , Diona
  , Fischl
  , Ganyu
  , HuTao
  , Kaeya
  , Keqing
  , KujouSara
  , Lisa
  , Ningguang
  , Noelle
  , Qiqi
  , RaidenShogun
  , Razor
  , Rosaria
  , Sayu
  , Shenhe
  , Sucrose
  , Thoma
  , Traveler
  , Xiangling
  , Xingqiu
  , Xinyan
  , YaeMiko
  , Yanfei
  , YunJin
  , Zhongli
  ]

defaultAccount :: Account
defaultAccount = Set.fromList
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

data Conf = Conf
  { _databasePath    :: FilePath
  , _accountPath :: FilePath
  , _outputPath  :: FilePath
  }
  deriving (Eq, Show)

defaultConf :: Conf
defaultConf = Conf "./data/data.csv" "./data/account.csv" "./data/output.csv"

$(makeClassy ''Conf)
