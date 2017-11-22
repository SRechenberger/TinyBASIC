module TinyBASIC.Execution where

import TinyBASIC.Definition
import TinyBASIC.Parser

import Text.Parsec (parse, ParseError)

import Control.Monad.State
import Control.Monad.Except

import Control.Applicative ((<|>))

import Data.Map (Map)
import qualified Data.Map as Map

import System.IO
import System.IO.Error
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
  deriving (Eq, Show)

newExec :: Exec
newExec = Exec
  { listing =  Map.empty
  , pc = 0
  , vars = Map.empty
  , rets = []
  , mode = COMMAND
  }

data MODE = COMMAND | PROGRAM | TERMINATE
  deriving(Eq, Show)

type Run a = StateT Exec (ExceptT String IO) a

runRun :: Run a -> Exec -> IO (Either String (a, Exec))
runRun action exec = runExceptT (runStateT action exec)

evalRun :: Run a -> Exec -> IO (Either String a)
evalRun action exec = runExceptT (evalStateT action exec)

execRun :: Run a -> Exec -> IO (Either String Exec)
execRun action exec = runExceptT (execStateT action exec)

guardNumber :: Expr -> Run Word
guardNumber (Number n) = pure n
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
    Mod -> mod

eval (Un o e) = do
  e' <- eval e >>= guardNumber
  pure $ Number (o' e')
 where
  o' = case o of
    Sub -> negate
    Add -> id

eval prim = pure prim

ppAtom :: Expr -> Run String
ppAtom (Str s) = pure s
ppAtom (Number n) = pure $ pp n
ppAtom e = do
  e' <- eval e
  ppAtom e'

command :: Stmt -> Run MODE
command (PRINT es) = do
  mapM_ (eval >=> ppAtom >=> (liftIO . putStr)) es
  liftIO $ putStrLn ""
  gets mode
command (IF l o r s) = do
  l' <- eval l >>= guardNumber
  r' <- eval r >>= guardNumber
  if (o' l' r') then command s else gets mode
 where
  o' = case o of
    Lt -> (<)
    Gt -> (>)
    Eq -> (==)
    Leq -> (<=)
    Geq -> (>=)
    Neq -> (/=)
command (GOTO e) = do
  m <- gets mode
  if (m == PROGRAM)
    then do
      e' <- eval e >>= guardNumber
      modify (\s -> s {pc = e'})
      gets mode
    else do
      gets mode
command (INPUT vs) = do
  forM_ vs $ \v -> do
    liftIO $ do
      putStr "? "
      hFlush stdout
    l <- parse ((Str <$> str) <|> (Number <$> number)) "" <$> liftIO getLine
    case l of 
      Left e -> throwError (show e)
      Right i -> modify (\s -> s {vars = Map.insert v i (vars s)})
  gets mode
command (LET v e) = do
  e' <- eval e
  modify (\s -> s {vars = Map.insert v e' (vars s)})
  gets mode
command (GOSUB e) = do
  m <- gets mode
  when (m == PROGRAM) $ do
    e' <- eval e >>= guardNumber
    modify (\s -> s
      { pc = e'
      , rets = (pc s + 1) : rets s 
      })
  gets mode
command RETURN = do
  ret <- gets rets
  case ret of
    [] -> throwError "Empty return stack."
    x:xs -> modify (\s -> s
      { pc = x
      , rets = xs
      })
  gets mode
command CLEAR = gets mode
command LIST = do
  lst <- gets $ Map.toList . listing
  forM_ lst $ \(l,s) -> do
    liftIO $ putStrLn $ show l ++ " " ++ pp s
  gets mode
command RUN = do
  modify $ \s -> s { mode = PROGRAM, pc = 0 }
  pure PROGRAM
command END = do
  m <- gets mode
  case m of
    COMMAND -> do
      modify $ \s -> s { mode = TERMINATE }
      pure TERMINATE
    PROGRAM -> do
      modify $ \s -> s { mode = COMMAND }
      pure COMMAND
    TERMINATE -> pure TERMINATE

execute :: [Either ParseError LstLine] -> MODE -> Run ()
execute [] _ = pure ()
execute _ TERMINATE = pure ()
execute (l:ls) COMMAND = do
  m <- case l of
    Left e -> do
      liftIO $ putStrLn $ "Parser Error: " ++ show e
      pure COMMAND
    Right (Lst l stmt) -> do
      modify $ \s -> s
        { listing = Map.insert l stmt (listing s) }
      pure COMMAND
    Right (Cmd s)      -> do
      command s
  execute ls m
execute ls PROGRAM = do
  s <- gets $ \s -> Map.lookup (pc s) (listing s)
  m <- case s of
    Nothing -> do
      modify $ \s -> s { pc = pc s + 1 }
      pure PROGRAM
    Just  s -> do
      m' <- command s
      modify $ \s -> s { pc = pc s + 1 }
      pure m'
  execute ls m
