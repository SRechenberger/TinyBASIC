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
import System.Random
import Control.Monad.Random

--------------------------------------------------------------------------------
-- Utils -----------------------------------------------------------------------
--------------------------------------------------------------------------------

fib :: Word -> Word
fib = fib' 0 1 
 where
  fib' r1 _  0 = r1
  fib' r1 r2 n = fib' r2 (r1+r2) (n-1) 

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
        exec' <- processStmt l (exec & pc +~ 1)
        runAutomatically prg (exec')
  TERMINATE -> pure exec


makeTest :: String -> Exec -> Exec -> Test
makeTest input initState expectedState = TestCase $ do
  let prg = parse (many line) "" input
  case prg of
    Left err -> assertFailure $
      "Parse Error: " ++ show err
    Right prg' -> case runAutomatically prg' initState of
      Left err -> assertFailure $ 
        "Execution Error: " ++ err
      Right endState -> assertBool
        ("States do not match: "
         ++ "expected: \n"
         ++ "  " ++ show expectedState ++ "\n"
         ++ "got: \n"
         ++ "  " ++ show endState)
        (expectedState == endState)


makeRandomTest1 :: (Random a) => Int -> IO a -> (a -> String) -> (a -> Exec) -> (a -> Exec) -> Test
makeRandomTest1 n gen inputGen initStateGen expectedStateGen = TestList $ replicate n $ TestCase $ do
  rand1 <- gen
  let prg = parse (many line) "" (inputGen rand1)
  let initState = initStateGen rand1
  let expectedState = expectedStateGen rand1
  case prg of
    Left err -> assertFailure $
      "Parse Error: " ++ show err
    Right prg' -> case runAutomatically prg' initState of
      Left err -> assertFailure $ 
        "Execution Error: " ++ err
      Right endState -> assertBool
        ("States do not match: "
         ++ "expected: \n"
         ++ "  " ++ show expectedState ++ "\n"
         ++ "got: \n"
         ++ "  " ++ show endState)
        (expectedState == endState)


makeRandomTest2 :: (Random a, Random b) => IO a -> IO b -> (a -> b -> String) -> (a -> b -> Exec) -> (a -> b -> Exec) -> Test
makeRandomTest2 gen1 gen2 inputGen initStateGen expectedStateGen = TestCase $ do
  rand1 <- gen1
  rand2 <- gen2
  let prg = parse (many line) "" (inputGen rand1 rand2)
  let initState = initStateGen rand1 rand2
  let expectedState = expectedStateGen rand1 rand2
  case prg of
    Left err -> assertFailure $
      "Parse Error: " ++ show err
    Right prg' -> case runAutomatically prg' initState of
      Left err -> assertFailure $ 
        "Execution Error: " ++ err
      Right endState -> assertBool
        ("States do not match: "
         ++ "expected: \n"
         ++ "  " ++ show expectedState ++ "\n"
         ++ "got: \n"
         ++ "  " ++ show endState)
        (expectedState == endState)


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

listings :: Test
listings = TestLabel "Listings" $ 
  TestList $
    [ makeTest
        "10 PRINT 'Hello World'\n20 END\n RUN\nEND"
        newExec
        (newExec
          & pc .~ 21
          & listing .~
              Map.fromList
                [(10, PRINT [Str "Hello World"])
                ,(20, END)]
          & mode .~ TERMINATE
          & screen .~ ["Hello World"])
    , makeRandomTest1 1000
        (getRandomR (1,100))
        (\i ->
          "10     LET A = 0\n"
          ++ "20  LET B = 1\n"
          ++ "30  LET N = " ++ show i ++ "\n"
          ++ "40  IF N <= 0 THEN GOTO 100\n"
          ++ "50  LET Tmp = A + B\n"
          ++ "60  LET A = B\n"
          ++ "70  LET B = Tmp\n"
          ++ "80  LET N = N - 1\n"
          ++ "90  GOTO 40\n"
          ++ "100 PRINT A\n"
          ++ "110 END\n"
          ++ "RUN\n"
          ++ "END")
        (const newExec)
        (\i -> newExec
          & pc .~ 111
          & listing .~
              Map.fromList
                [ (10, LET "A" (Number 0))
                , (20, LET "B" (Number 1))
                , (30, LET "N" (Number i))
                , (40, IF (Var "N") Leq (Number 0) (GOTO (Number 100)))
                , (50, LET "Tmp" (Bin Add (Var "A") (Var "B")))
                , (60, LET "A" (Var "B"))
                , (70, LET "B" (Var "Tmp"))
                , (80, LET "N" (Bin Sub (Var "N") (Number 1)))
                , (90, GOTO (Number 40))
                , (100, PRINT [Var "A"])
                , (110, END)]
          & mode .~ TERMINATE
          & screen .~ [show (fib i)]
          & vars .~
              Map.fromList
                [ ("A", Number $ fib i), ("B", Number $ fib (i+1)), ("N", Number 0), ("Tmp", Number $ fib (i+1)) ])
    , makeRandomTest1 1000
        getRandom
        (\i ->
          "10 INPUT X\n"
          "20 PRINT X\n"
          "30 END\n"
          "RUN\n"
          "RUN\n"
          "RUN\n")
        (\i -> newExec & inBuf .~ [Number i, Number (i+2), Number (i+4)])
        (\i -> newExec
          & pc .~ 31
          & listing .~
              Map.fromList
                [ (10, INPUT ["X"])
                , (20, PRINT [Var "X"])
                , (30, END)]
          & mode .~ COMMAND
          & screen .~ [Number $ show (i+4), Number $ show (i+2), Number $ show i]
          & vars .~ Map.fromList [("X", Number (i+4))])]
                


       


--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  c <- runTestTT $ test [singleStatement, listings]
  if errors c == 0 && failures c == 0
    then exitSuccess
    else exitFailure
