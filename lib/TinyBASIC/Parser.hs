module TinyBASIC.Parser where

import TinyBASIC.Definition

import Text.Parsec
import Text.Parsec.String
import Data.Char (isUpper, isDigit)

--------------------------------------------------------------------------------
-- Parser ----------------------------------------------------------------------
--------------------------------------------------------------------------------

number :: Parser Number
number = read <$> many1 digit

line :: Parser LstLine
line = do
  la <- lookAhead anyChar
  r <- case la of
    d | isDigit d -> do
      spaces
      l <- number
      spaces
      s <- statement
      spaces
      pure $ Lst l s
    d | isUpper d -> do
      spaces
      s <- statement
      spaces
      pure $ Cmd s
  pure r

symbol :: String -> Parser String
symbol s = spaces *> string s <* spaces

str :: Parser String
str = spaces *> between (char '\'') (char  '\'') (manyTill (try anyChar) (lookAhead $ char '\''))

var :: Parser Var
var = (:) <$> upper <*> many alphaNum

relop :: Parser Relop
relop = do
  spaces
  o1 <- oneOf "<>="
  o2 <- try (Just <$> oneOf "<>=") <|> pure Nothing
  case o2 of
    Nothing -> case o1 of
      '<' -> pure Lt
      '>' -> pure Gt
      '=' -> pure Eq
    Just o2' -> case o1:o2':[] of
      "<=" -> pure Leq
      ">=" -> pure Geq
      "<>" -> pure Neq
      o    -> fail $ "Unknown relop: " ++ show o

op :: Parser Op
op = do
  spaces
  o <- oneOf "+-*/%"
  case o of
    '+' -> pure Add
    '-' -> pure Sub
    '*' -> pure Mul
    '/' -> pure Div
    '%' -> pure Mod
    e   -> fail $ "Unkown op: " ++ show e

statement :: Parser Stmt
statement = do
  spaces
  key <- many1 upper
  spaces
  case key of
    "PRINT" -> do
      spaces
      elist <- sepBy expr (try $ symbol ",")
      pure $ PRINT elist
    "IF" -> do
      e1 <- expr
      spaces
      r <- relop
      spaces
      e2 <- expr
      spaces
      string "THEN"
      spaces
      s <- statement
      pure $ IF e1 r e2 s
    "GOTO" -> do
      spaces
      e <- expr
      pure $ GOTO e
    "INPUT" -> do
      vlist <- sepBy var (spaces *> try (char ',') <* spaces)
      pure $ INPUT vlist
    "LET" -> do
      v <- upper
      vs <- many $ upper <|> lower
      spaces
      char '='
      spaces
      e <- expr
      pure $ LET (v:vs) e
    "GOSUB" -> do
      e <- expr
      pure $ GOSUB e
    "RETURN" -> pure RETURN
    "CLEAR" -> pure CLEAR
    "LIST" -> pure LIST
    "RUN" -> pure RUN
    "END" -> pure END
    o     -> fail $ "Unexpected keyword: " ++ show o

expr :: Parser Expr
expr = do 
  spaces
  la <- lookAhead anyChar
  case la of
    '\'' -> Str <$> str
    _    -> expr'

expr' :: Parser Expr
expr' = do
  spaces
  o <- (Just <$> try op) <|> pure Nothing
  spaces
  t <- case o of
    Nothing -> term
    Just o  -> Un o <$> term
  spaces
  ts <- many ((,) <$> (op <* spaces) <*> (term <* spaces)) 
  pure $ foldl (\acc (o,t) -> Bin o acc t) t ts

term :: Parser Expr
term = do
  spaces
  f <- factor
  spaces
  fs <- many ((,) <$> (op <* spaces) <*> (factor <* spaces)) 
  pure $ foldl (\acc (o,f) -> Bin o acc f) f fs
  
factor :: Parser Expr
factor = do
  spaces
  la <- lookAhead anyChar
  case la of
    '(' -> between (char '(') (char ')') expr'
    u | isUpper u -> Var <$> var
    d | isDigit d -> Number <$> number
    o -> fail $ "Unexpected character: " ++ show o
    
