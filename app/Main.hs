module Main where

import           Control.Monad                  ( void )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Core                           ( app
                                                , getConf
                                                )

main :: IO ()
main = void $ runExceptT $ do
  env <- getConf
  runReaderT app env
