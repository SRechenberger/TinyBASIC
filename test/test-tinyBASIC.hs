module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import TinyBASIC.Definition
import TinyBASIC.Parser

eqTest :: (Show a, Eq a) => String -> a -> a -> Test
eqTest name actual expected = TestCase $ assertEqual name actual expected

parseTest :: (Show a, Eq a) => Parser a -> String -> String -> a -> Test 
parseTest p n s e = eqTest n (parse p "" s) (Right e)  


tests = test
  [ parseTest statement "stmt-1" "RUN" RUN
  , parseTest statement "stmt-2" "END" END
  , parseTest statement "stmt-3" "LIST" LIST
  , parseTest statement "stmt-3" "CLEAR" CLEAR
  , parseTest statement "stmt-3" "RETURN" RETURN
  ]

main :: IO ()
main = do
  c <- runTestTT tests
  -- print c
  if errors c == 0 && failures c == 0
  then exitSuccess
  else exitFailure
