{-# LANGUAGE LambdaCase #-}
module Main where

import Test.HUnit

import TinyBASIC.Definition
import TinyBASIC.Parser
import TinyBASIC.Execution

import Text.Parsec

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State
import Control.Lens

import System.Exit
import System.IO
import System.Process
import Control.Monad.Random.Class

--------------------------------------------------------------------------------
-- Test Suite ------------------------------------------------------------------
--------------------------------------------------------------------------------

runAutomatically :: [LstLine] -> Exec -> Either String Exec
runAutomatically [] exec = pure exec
runAutomatically prg@(l:ls) exec = case exec^.mode of
  COMMAND -> do
    exec' <- processLine l exec
    runAutomatically ls exec'
  PROGRAM -> case exec^.listing.at (exec^.pc) of
      Nothing -> runAutomatically prg (exec & pc +~ 1)
      Just l  -> do
        exec' <- processStmt l exec
        pure $ exec' & pc +~ 1
  TERMINATE -> pure exec

makeTest :: String -> Exec -> Exec -> Test
makeTest str execInit execExp = TestCase $ do
  let prg = parse (many line) "" str
  case prg of
    Left err -> assertFailure $
      "Parse Error: " ++ show err
    Right prg' -> case runAutomatically prg' execInit of
      Left err -> assertFailure $ 
        "Execution Error: " ++ err
      Right exec' -> assertBool
        ("States do not match: "
         ++ "expected: \n"
         ++ "  " ++ show execExp ++ "\n"
         ++ "got: \n"
         ++ "  " ++ show exec')
        (execExp == exec')

--------------------------------------------------------------------------------
-- Tests Cases -----------------------------------------------------------------
--------------------------------------------------------------------------------

singleStatement :: Test
singleStatement = TestLabel "Single Statement" $
  TestList $
    [ makeTest
        "PRINT 'Hello World'"
        newExec
        (newExec & screen .~ ["Hello World"])
    , makeTest
        "GOTO 10"
        newExec
        (newExec & pc .~ 10)
    , makeTest
        "LET X = 10"
        newExec
        (newExec & vars.at "X" ?~ Number 10)
    , makeTest
        "INPUT X"
        (newExec & inBuf .~ [Number 10])
        (newExec & vars.at "X" ?~ Number 10)
    , makeTest
        "INPUT X"
        newExec
        (newExec & rqInput .~ True)
    , makeTest
        "INPUT X, Y"
        (newExec & inBuf .~ [Number 10])
        (newExec
          & vars.at "X" ?~ Number 10
          & rqInput .~ True)
    ]

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  c <- runTestTT singleStatement
  if errors c == 0 && failures c == 0
    then exitSuccess
    else exitFailure
