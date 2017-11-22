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
  ls <- map (parse line "") . lines <$> getContents

  r <- runExceptT $ evalStateT (execute ls COMMAND) newExec
  case r of
    Left e -> do
      putStrLn "Execution Failed:"
      putStrLn e
      exitFailure
    Right () -> do
      exitSuccess
