module TinyBASIC.Execution where

import TinyBASIC.Definition

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Execution -------------------------------------------------------------------
--------------------------------------------------------------------------------

data Exec = Exec
  { listing :: Map LineNumber Statement
  , pc      :: LineNumber
  , vars    :: Map Var Expr
  }
  deriving Show

type Run a = StateT Exec IO a
