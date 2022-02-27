module Main where

import           Control.Monad                  ( void )
import           Control.Monad.Except           ( runExceptT )
import           Core                           ( app )

main :: IO ()
main = void $ runExceptT app
