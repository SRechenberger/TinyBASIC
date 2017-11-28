{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module TinyBASIC.Execution where

import Control.Lens

import TinyBASIC.Definition
import TinyBASIC.Parser

import Text.Parsec (parse, ParseError)

import Control.Applicative ((<|>))
import Control.Monad (when, (<=<))

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Utils -----------------------------------------------------------------------
--------------------------------------------------------------------------------

for :: [a] -> (a -> b) -> [b]
for = flip map

--------------------------------------------------------------------------------
-- Execution -------------------------------------------------------------------
--------------------------------------------------------------------------------

data MODE = COMMAND | PROGRAM | TERMINATE
  deriving(Eq, Show)

data Exec = Exec
  { _listing :: Map LineNumber Stmt
  , _pc      :: Word
  , _vars    :: Map Var Expr
  , _rets    :: [Word]
  , _mode    :: MODE
  , _inBuf   :: [Expr]
  , _rqInput :: Bool
  , _screen  :: [String]
  }
  deriving (Eq, Show)

makeLenses ''Exec

newExec :: Exec
newExec = Exec
  { _listing =  Map.empty
  , _pc = 0
  , _vars = Map.empty
  , _rets = []
  , _mode = COMMAND
  , _inBuf = []
  , _rqInput = False
  , _screen = []
  }


processLine :: LstLine -> Exec -> Either String Exec
processLine (Cmd stmt) exec = processStmt stmt exec
processLine (Lst l stmt) exec = Right $ exec
  & listing.at l ?~ stmt


processStmt :: Stmt -> Exec -> Either String Exec
processStmt (PRINT exprs) exec = do
  exprs' <- mapM (pure . prn <=< eval exec) exprs
  pure $ exec 
    & screen %~ (concat exprs':)
 where
  prn (Str s) = s
  prn (Number n) = show n

processStmt (IF e1 op e2 s) exec = do
  e1' <- evalNum exec e1
  e2' <- evalNum exec e2
  if e1' `op'` e2'
    then processStmt s exec
    else pure exec
 where
  op' = case op of
    Lt -> (<)
    Gt -> (>)
    Leq -> (<=)
    Geq -> (>=)
    Eq -> (==)
    Neq -> (/=)

processStmt (GOTO e) exec = do
  e' <- evalNum exec e
  pure $ exec & pc .~ e'

processStmt (INPUT idents) exec = do
  let assgns = zip idents (exec ^. inBuf)
  pure $ exec
    & (\exec -> foldl (\exec (v,e) -> exec & vars.at v ?~ e) exec assgns)
    & inBuf %~ drop (length assgns)
    & rqInput .~ (length idents > length (exec^.inBuf))

processStmt (LET v e) exec = do
  e' <- eval exec e
  pure $ exec
    & vars.at v ?~ e'

processStmt (GOSUB e) exec = do
  e' <- evalNum exec e
  pure $ exec
    & pc .~ e'
    & rets %~ ((exec^.pc):)

processStmt RETURN exec = case exec^.rets of
    [] -> Left "No return addresses."
    r:rs -> pure $ exec
      & pc .~ r
      & rets .~ rs

processStmt CLEAR exec = pure $ exec & screen .~ []

processStmt LIST exec = pure $ exec
  & screen %~ (lst ++)
 where
  lst :: [String]
  lst = reverse $ for (Map.toList (exec^.listing)) (\(l,s) -> show l ++ " " ++ pp s)

processStmt RUN exec = pure $ exec
  & pc .~ 0
  & mode .~ PROGRAM

processStmt END exec = pure $ exec
  & mode .~ (case exec^.mode of
      PROGRAM -> COMMAND
      COMMAND -> TERMINATE
      TERMINATE -> TERMINATE)


eval :: Exec -> Expr -> Either String Expr
eval exec e = eval' e
 where
  eval' (Var v) = case exec^.vars.at v of
    Nothing -> Left $ "Variable " ++ show v ++ " not found."
    Just e  -> pure e
  eval' (Number n) = pure $ Number n
  eval' (Str s)    = pure $ Str s
  eval' (Bin o e1 e2) = do
    e1' <- evalNum exec e1
    e2' <- evalNum exec e2
    let o' = case o of
              Add -> (+)
              Sub -> (-)
              Mul -> (*)
              Div -> div
              Mod -> mod
    pure $ Number $ e1' `o'` e2'
  eval' (Un o e) = do
    e' <- evalNum exec e
    o' <- case o of
            Add -> pure id
            Sub -> pure negate
            o   -> Left $ "No legal unary operator: " ++ pp o
    pure $ Number $ o' e'


evalNum :: Exec -> Expr -> Either String Word
evalNum exec e = do
  e' <- eval exec e
  case e' of
    Str s -> Left $ "No a number: " ++ pp e'
    Number n -> pure n
