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

data TestCase a = TestCase String a
  deriving (Eq, Show)

instance (PP a, Arbitrary a) => Arbitrary (TestCase a) where
  arbitrary = do
    a <- arbitrary
    pure $ TestCase (pp a) a

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
    , INPUT <$> listOf ident
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

string :: Gen String
string = do
  l <- choose (0,100)
  vectorOf l (choose ('A','z'))

noStr :: Gen Expr
noStr = do
  e <- arbitrary
  case e of 
    Str _ -> noStr
    e'    -> pure e

instance Arbitrary Expr where
  arbitrary = oneof
    [ Var <$> ident
    , Number <$> arbitrary
    , Str <$> string
    , Bin <$> arbitrary <*> noStr <*> noStr
    , Un <$> elements [Add,Sub] <*> noStr
    ]
 
instance Arbitrary Relop where
  arbitrary = oneof (map pure [Lt .. Neq])

instance Arbitrary Op where
  arbitrary = oneof (map pure [Add .. Mod])
        

--------------------------------------------------------------------------------
-- Tests -----------------------------------------------------------------------
--------------------------------------------------------------------------------

parseCorrectly :: (Eq a, PP a) => Parser a -> TestCase a -> Bool
parseCorrectly parser (TestCase s ast) = case parse parser "" s of
  Right ast' -> ast == ast'
  Left _     -> False


checkLine, checkNumber, checkRelop, checkOp, checkStatement, checkExpr :: IO Result
checkLine = quickCheckResult $ parseCorrectly line
checkNumber = quickCheckResult $ parseCorrectly number
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
    , checkStatement
    , checkNumber
    , checkRelop
    , checkOp
    ]
  if r then exitSuccess else exitFailure
