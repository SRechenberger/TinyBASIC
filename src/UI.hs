{-# LANGUAGE OverloadedStrings #-}
module UI where

import Brick
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.Edit as Edit
import Brick.Util (fg)

import Brick.AttrMap

import Graphics.Vty (defAttr, Event (..), Key (..), blue)

import TinyBASIC.Definition
import TinyBASIC.Execution
import TinyBASIC.Parser (line)

import Text.Parsec (parse)

import Control.Lens
import Data.Monoid

data UIState = UIState Exec (Editor String ())


drawUI :: UIState -> [Widget ()]
drawUI (UIState exec edit) = pure $
  (withAttr "blue" . padBottom Max . padRight Max . str . unlines . reverse $ exec ^. screen)
  <=> (Edit.renderEditor (str . unlines) True edit)


app = App
  { appDraw = drawUI
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

theMap :: AttrMap
theMap = attrMap defAttr [("blue", bg blue)]

handleEvent :: UIState -> BrickEvent () e -> EventM () (Next UIState)
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
handleEvent (UIState exec edit) (VtyEvent (EvKey KEnter [])) = do
  let inp = parse line "" . head . Edit.getEditContents $ edit
  
  let exec' = case inp of
                Left err -> exec & screen %~ (show err:)
                Right l  -> case processLine l exec of
                  Left err    -> exec & screen %~ (show err:)
                  Right exec' -> exec'
  continue $ UIState exec' (Edit.editor () Nothing "")
handleEvent (UIState exec edit) (VtyEvent event) = Edit.handleEditorEvent event edit >>= continue . UIState exec

