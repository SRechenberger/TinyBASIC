module Main where

import Test.HUnit

import TinyBASIC.Definition
import TinyBASIC.Parser
import TinyBASIC.Execution

import Text.Parsec

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except

test1 :: (String, Exec)
test1 =
  ( "LET A = 10"
  , newExec { vars = Map.fromList [("A", Number 10)] }
  )

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


main :: IO ()
main = do
  c <- runTestTT $ test [makeTest test1]
  print c
