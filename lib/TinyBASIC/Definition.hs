module TinyBASIC.Definition where

import Data.Word (Word)
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- Definitions -----------------------------------------------------------------
--------------------------------------------------------------------------------

type LineNumber = Word
type Var = String
type Number = Word

data LstLine
  = Lst LineNumber Stmt
  | Cmd Stmt
  deriving(Eq)

instance Show LstLine where
  show (Lst n s) = show n ++ " " ++ show s ++ "\n"
  show (Cmd s)   = show s ++ "\n"

data Stmt
  = PRINT [Expr]
  | IF Expr Relop Expr Stmt
  | GOTO Expr
  | INPUT [Var]
  | LET Var Expr
  | GOSUB Expr
  | RETURN
  | CLEAR
  | LIST
  | RUN
  | END
  deriving(Eq)

instance Show Stmt where
  show (PRINT es)   = "PRINT " ++ intercalate ", " (map show es) ++ "\n"
  show (IF l o r s) = "IF " ++ show l ++ " " ++ show o ++ " " ++ show r ++ " THEN " ++ show s ++ "\n"
  show (GOTO e)     = "GOTO " ++ show e ++ "\n"
  show (INPUT vs)   = "INPUT " ++ intercalate ", " vs ++ "\n"
  show (LET v e)    = "LET " ++ v ++ " = " ++ show e ++ "\n"
  show (GOSUB e)    = "GOSUB " ++ show e ++ "\n"
  show RETURN       = "RETURN\n"
  show CLEAR        = "CLEAR\n"
  show LIST         = "LIST\n"
  show RUN          = "RUN\n"
  show END          = "END\n"

data Expr
  = Var Var
  | Number Number
  | Str String
  | Bin Op Expr Expr
  | Un Op Expr
  deriving(Eq)

instance Show Expr where
  show (Var v)     = v
  show (Number n)  = show n
  show (Str s)     = "\'" ++ s ++ "\'"
  show (Bin o l r) = "(" ++ show l ++ " " ++ show o ++ " " ++ show r ++ ")"
  show (Un o e)    = show o ++ show e 

data Relop
  = Lt | Gt
  | Eq | Leq | Geq
  | Neq
  deriving(Eq,Enum)

instance Show Relop where
  show Lt = "<"
  show Gt = ">"
  show Eq = "="
  show Leq = "<="
  show Geq = ">="
  show Neq = "<>"

data Op
  = Add | Sub | Mul | Div
  deriving(Eq,Enum)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

