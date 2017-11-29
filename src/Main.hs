module Main where

import TinyBASIC.Definition
import TinyBASIC.Parser
import TinyBASIC.Execution

import UI
import Brick
import qualified Brick.Widgets.Edit as Edit

import System.Exit

main :: IO ()
main = do
  defaultMain app (UIState newExec (Edit.editor () Nothing ""))
  pure ()
