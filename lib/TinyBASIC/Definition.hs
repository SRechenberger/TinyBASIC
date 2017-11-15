module TinyBASIC.Definition where

import Data.Word (Word)
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- Definitions -----------------------------------------------------------------
--------------------------------------------------------------------------------

class PP a where
  pp :: a -> String

type LineNumber = Word
type Var = String
type Number = Word

instance PP Word where
  pp = show
  
data LstLine
  = Lst LineNumber Stmt
  | Cmd Stmt
  deriving(Eq, Show)

instance PP LstLine where
  pp (Lst n s) = pp n ++ " " ++ pp s
  pp (Cmd s)   = pp s

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
  deriving(Eq, Show)

instance PP Stmt where
  pp (PRINT es)   = "PRINT " ++ intercalate ", " (map pp es) 
  pp (IF l o r s) = "IF " ++ pp l ++ " " ++ pp o ++ " " ++ pp r ++ " THEN " ++ pp s
  pp (GOTO e)     = "GOTO " ++ pp e
  pp (INPUT vs)   = "INPUT " ++ intercalate ", " vs
  pp (LET v e)    = "LET " ++ v ++ " = " ++ pp e
  pp (GOSUB e)    = "GOSUB " ++ pp e
  pp RETURN       = "RETURN"
  pp CLEAR        = "CLEAR"
  pp LIST         = "LIST"
  pp RUN          = "RUN"
  pp END          = "END"

data Expr
  = Var Var
  | Number Number
  | Str String
  | Bin Op Expr Expr
  | Un Op Expr
  deriving(Eq, Show)

instance PP Expr where
  pp (Var v)     = v
  pp (Number n)  = pp n
  pp (Str s)     = "\'" ++ s ++ "\'"
  pp (Bin o l r) = "(" ++ pp l ++ " " ++ pp o ++ " " ++ pp r ++ ")"
  pp (Un o e)    = "(" ++ pp o ++ pp e ++ ")"

data Relop
  = Lt | Gt
  | Eq | Leq | Geq
  | Neq
  deriving(Eq,Enum, Show)

instance PP Relop where
  pp Lt = "<"
  pp Gt = ">"
  pp Eq = "="
  pp Leq = "<="
  pp Geq = ">="
  pp Neq = "<>"

data Op
  = Add | Sub | Mul | Div
  deriving(Eq,Enum,Show)

instance PP Op where
  pp Add = "+"
  pp Sub = "-"
  pp Mul = "*"
  pp Div = "/"

