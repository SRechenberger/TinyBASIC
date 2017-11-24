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

import System.Exit
import System.IO
import System.Process
import Control.Monad.Random.Class

--------------------------------------------------------------------------------
-- Test Suite ------------------------------------------------------------------
--------------------------------------------------------------------------------

makeTest :: (String, Exec) -> Test
makeTest (str, state) = TestCase $ do
  let prg = map (parse line "") . lines $ str
  estate' <- execRun (execute prg COMMAND) newExec
  case estate' of
    Left _ -> do
      assertFailure
        $ "CASE:\n"
        ++ str
        ++ "\nExecution Failed."
    Right state' -> do
      assertBool
        ("States don't match:\n\t"
        ++ show state ++ "\n\t"
        ++ show state' ++ "\n\t")
        (state == state')

--------------------------------------------------------------------------------
-- Tests Cases -----------------------------------------------------------------
--------------------------------------------------------------------------------

test1, test2, test3 :: (String, Exec)
test1 = ("LET A = 10", newExec { vars = Map.fromList [("A", Number 10)] })

test2 = ("10 END\nRUN", newExec { mode = PROGRAM })

test3 =
  ("10 END\nRUN\nEND"
  , newExec
    { listing = Map.fromList [(10, END)]
    , pc = 11
    , mode = TERMINATE})

test4 = ("END", newExec { mode = TERMINATE })

test5 =
  ( "10 PRINT 1\n20 PRINT 2"
  , newExec
    { listing = Map.fromList
      [ (10, PRINT [Number 1] )
      , (20, PRINT [Number 2] )
      ]
    }
  )

gcd' :: Word -> Word -> (String, Exec)
gcd' a b =
  ( "10 LET A = " ++ show a ++ "\n"
    ++ "20 LET B = " ++ show b ++ "\n"
    ++ "30 IF B = 0 THEN GOTO 70\n"
    ++ "40 LET H = A % B\n"
    ++ "50 LET A = B\n"
    ++ "60 LET B = H\n"
    ++ "70 GOTO 30\n"
    ++ "80 END\n"
    ++ "RUN\n"
    ++ "END"
  , newExec
    { listing = Map.fromList
      [ (10, LET "A" (Number a))
      , (20, LET "B" (Number b))
      , (30, IF (Var "B") Neq (Number 0) (GOTO (Number 70)))
      , (40, LET "H" (Bin Mod (Var "A") (Var "B")))
      , (50, LET "A" (Var "B"))
      , (60, LET "B" (Var "H"))
      , (70, GOTO (Number 30))
      , (80, END)
      ]
    , pc = 81
    , vars = Map.fromList
      [ ("A", Number $ gcd a b)
      , ("B", Number 0)
      , ("H", Number $ gcd a b)
      ]
    , rets = []
    , mode = TERMINATE
    }
  )

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  gcdArgs <- take 10 <$> (zip <$> getRandoms <*> getRandoms)
  let gcdTest = map (makeTest . uncurry gcd') gcdArgs
  c <- runTestTT . test $
    [ makeTest test1
    , makeTest test2
    , makeTest test3
    , makeTest test4
    , makeTest test5
    ]-- ++ gcdTest
  if errors c == 0 && failures c == 0
    then exitSuccess
    else exitFailure
