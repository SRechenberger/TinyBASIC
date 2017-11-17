module Main where

import TinyBASIC.Definition
import TinyBASIC.Parser
import TinyBASIC.Execution

import System.Exit
import System.IO

import Control.Monad.State
import Control.Monad.Except

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  r <- runExceptT $ evalStateT execute newExec
  case r of
    Left e -> do
      putStrLn "Execution Failed:"
      putStrLn e
      exitFailure
    Right () -> do
      exitSuccess
