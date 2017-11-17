module TinyBASIC.Execution where

import TinyBASIC.Definition

import Control.Monad.State
import Control.Monad.Error

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Execution -------------------------------------------------------------------
--------------------------------------------------------------------------------

data Exec = Exec
  { listing :: Map LineNumber Stmt
  , pc      :: Word
  , vars    :: Map Var Expr
  , rets    :: [Word]
  , mode    :: MODE
  }
  deriving Show

data MODE = COMMAND | PROGRAM

type Run a = StateT Exec (ExceptT String IO) a

guardNumber :: Expr -> Run Word
guardNumber (Num n) = pure n
guardNumber e = throwError $ "Not a number: " ++ pp e

eval :: Expr -> Run Expr
eval (Var v) = do
  mv <- gets $ Map.lookup v . vars
  case mv of
    Nothing -> throwError $ "Variable " ++ v ++ " unbound."
    Just v' -> pure v'

eval (Bin o l r) = do
  l' <- eval l >>= guardNumber
  r' <- eval r >>= guardNumber
  pure $ Number $ l' `o'` r'
 where
  o' = case o of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> div

eval (Un o e) = do
  e' <- eval e >>= guardNumber
  pure $ Number (o' e')
 where
  o' = case o of
    Sub -> negate
    Add -> id

eval prim = pure prim


command :: Stmt -> Run ()
command (PRINT es) = mapM (eval >=> (pure . show) >=> (liftIO . putStr)) es
command (IF l o r s) = do
  l' <- eval l >>= guardNumber
  r' <- eval r >>= guardNumber
  when (o' l' r')
    (command s)
 where
  o' = case o of
    Lt -> (<)
    Gt -> (>)
    Eq -> (==)
    Leq -> (<=)
    Geq -> (>=)
    Neq -> (/=)
command (GOTO e) = do
  mode <- gets mode
  when (mode == PROGRAM) $ do
    e' <- eval e >>= guardNumber
    modify (\s -> s {pc = e'})
command (INPUT vs) = do
  forM_ vs $ \v -> do
    putStr "? "
    l <- parse ((Str <$> str) <|> (Number <$> number)) "" <$> getLine
    case l of 
      Left e -> throwError e
      Right i -> modify (\s -> s {vars = Map.insert v l (vars s)})
command (LET v e) = do
  e' <- eval e
  modify (\s -> s {vars = Map.inser v e' (vars s)})
command (GOSUB e) = do
  m <- gets mode
  when (mode == PROGRAM) $ do
    e' <- eval e >>= guardNumber
    modify (\s -> s
      { pc = e'
      , rets = vars s + 1
      })
command RETURN = do
  ret <- gets rets
  case rest of
    [] -> throwError "Empty return stack."
    x:xs -> modify (\s -> s
      { pc = x
      , rets = xs
      })
command CLEAR = pure ()
command LIST = do
  lst <- gets $ Map.toList . listing
  forM_ lst $ \(l,s) -> do
    putStrLn $ show l ++ " " ++ pp s
command RUN = modify $ \s -> s { mode = PROGRAM, pc = 0 }
command END = modify $ \s -> s { mode = COMMAND }


