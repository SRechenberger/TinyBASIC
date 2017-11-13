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
  deriving(Eq,Show)

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
  deriving(Eq,Show)

data Expr
  = Var Var
  | Number Number
  | Str String
  | Bin Op Expr Expr
  | Un Op Expr
  deriving(Eq,Show)

data Relop
  = Lt | Gt
  | Eq | Leq | Geq
  | Neq
  deriving(Eq,Show)

data Op
  = Add | Sub | Mul | Div
  deriving(Eq,Show)



