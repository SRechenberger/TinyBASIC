module Main where

import Data.List (intercalate)

import System.Exit (exitFailure, exitSuccess)

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import TinyBASIC.Definition
import TinyBASIC.Parser

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Test (isSuccess, quickCheckResult, Result)
import Test.QuickCheck.Property (Testable)

--------------------------------------------------------------------------------
-- Arbitrary Instances ---------------------------------------------------------
--------------------------------------------------------------------------------

instance Arbitrary LstLine where
  arbitrary = oneof
    [ Lst <$> choose (0,1000) <*> arbitrary
    , Cmd <$> arbitrary
    ]

instance Arbitrary Stmt where
  arbitrary = oneof
    [ PRINT <$> arbitrary
    , IF <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , GOTO <$> arbitrary
    , INPUT <$> arbitrary
    , LET <$> ident <*> arbitrary
    , GOSUB <$> arbitrary
    , pure RETURN
    , pure CLEAR
    , pure LIST
    , pure RUN
    , pure END
    ]
    
ident :: Gen String
ident = do
  l <- choose (0,10)
  (:) <$> choose ('A','Z') <*> vectorOf l (choose ('a','z'))

instance Arbitrary Expr where
  arbitrary = oneof
    [ Var <$> ident
    , Number <$> arbitrary
    , Str <$> arbitrary
    , Bin <$> arbitrary <*> arbitrary <*> arbitrary
    , Un <$> elements [Add,Sub] <*> arbitrary
    ]
 
instance Arbitrary Relop where
  arbitrary = oneof (map pure [Lt .. Neq])

instance Arbitrary Op where
  arbitrary = oneof (map pure [Add .. Div])
        

--------------------------------------------------------------------------------
-- Tests -----------------------------------------------------------------------
--------------------------------------------------------------------------------

parseCorrectly :: (Eq a, Show a) => Parser a -> a -> Bool
parseCorrectly parser ast = case parse parser "" (show ast) of
  Right ast' -> ast == ast'
  Left _     -> False


checkLine, checkNumber, checkVar, checkRelop, checkOp, checkStatement, checkExpr :: IO Result
checkLine = quickCheckResult $ parseCorrectly line
checkNumber = quickCheckResult $ parseCorrectly number
checkVar = quickCheckResult $ parseCorrectly var
checkRelop = quickCheckResult $ parseCorrectly relop
checkOp = quickCheckResult $ parseCorrectly op
checkStatement = quickCheckResult $ parseCorrectly statement
checkExpr = quickCheckResult $ parseCorrectly expr

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------
--------------------------------------------------------------------------------

checks :: [IO Result] -> IO Bool
checks iors = and <$> mapM (fmap isSuccess) iors

main :: IO ()
main = do
  r <- checks  
    [ checkLine
    , checkExpr
    ]
  if r then exitSuccess else exitFailure
