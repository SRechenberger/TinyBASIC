module TinyBASIC.Definition where

import Data.Word (Word)

--------------------------------------------------------------------------------
-- Definitions -----------------------------------------------------------------
--------------------------------------------------------------------------------

type LineNumber = Word
type Var = String
type Number = Word

data LstLine
  = Lst LineNumber Stmt
  | Cmd Stmt
  deriving Show

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
  deriving Show

data Expr
  = Var Var
  | Number Number
  | Str String
  | Bin Op Expr Expr
  | Un Op Expr
  deriving Show

data Relop
  = Lt | Gt
  | Eq | Leq | Geq
  | Neq
  deriving Show

data Op
  = Add | Sub | Mul | Div
  deriving Show



