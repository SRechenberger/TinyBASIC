module Main where

import Test.HUnit

import TinyBASIC.Definition
import TinyBASIC.Parser
import TinyBASIC.Execution

import Text.Parsec

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except

import System.Exit

--------------------------------------------------------------------------------
-- Test Suite ------------------------------------------------------------------
--------------------------------------------------------------------------------

makeTest :: (String, Exec) -> Test
makeTest (str, state) = TestCase $ do
  estate' <- execRun (runTest str) newExec
  case estate' of
    Left _ -> assertFailure
      $ "CASE:\n"
     ++ str
     ++ "\nExecution Failed."
    Right state' -> assertBool
      ("States don't match:\n\t" 
      ++ show state ++ "\n\t"
      ++ show state' ++ "\n\t")
      (state == state')

runTest :: String -> Run ()
runTest str = do
  let prg = parse (many1 line) "" str
  case prg of
    Left e -> throwError (show e)
    Right prg' -> mapM_ (\(Cmd s) -> command s) prg'


--------------------------------------------------------------------------------
-- Tests Cases -----------------------------------------------------------------
--------------------------------------------------------------------------------

test1, test2, test3 :: (String, Exec)
test1 = ( "LET A = 10", newExec { vars = Map.fromList [("A", Number 10)] } )

test2 = ( "RUN", newExec { mode = PROGRAM } )

test3 = ( "RUN\nEND", newExec )

test4 = ( "END", newExec { mode = TERMINATE } )

test5 =
  ( "10 PRINT 1\n20 PRINT 2"
  , newExec
    { listing = Map.fromList
      [ (10, PRINT [Number 1] )
      , (20, PRINT [Number 2] ) ] } )

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  c <- runTestTT $ test
    [ makeTest test1
    , makeTest test2
    , makeTest test3
    , makeTest test4
    , makeTest test5
    ]
  if errors c == 0 && failures c == 0
    then exitSuccess
    else exitFailure
