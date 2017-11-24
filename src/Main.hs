module Main where

import TinyBASIC.Definition
import TinyBASIC.Parser
import TinyBASIC.Execution

import System.Exit
import System.IO

import Control.Monad.State
import Control.Monad.Except
import Text.Parsec

main :: IO ()
main = do
  putStrLn "Fuck You!"
  exitFailure
