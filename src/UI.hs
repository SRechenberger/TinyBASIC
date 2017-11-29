module UI where

import Brick
import Brick.Widgets.Edit (Editor)
import qualified Brick.Widgets.Edit as Edit

import Brick.AttrMap

import Graphics.Vty (defAttr, Event (..), Key (..))

import TinyBASIC.Definition
import TinyBASIC.Execution

import Control.Lens
import Data.Monoid

data UIState = UIState Exec (Editor String ())


drawUI :: UIState -> [Widget ()]
drawUI (UIState exec edit) =
  map str (exec ^. screen)
  <> [Edit.renderEditor (str . unlines) True edit]


app = App
  { appDraw = drawUI
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

theMap :: AttrMap
theMap = attrMap defAttr []

handleEvent :: UIState -> BrickEvent () e -> EventM () (Next UIState)
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
handleEvent (UIState exec edit) (VtyEvent event) = Edit.handleEditorEvent event edit >>= continue . UIState exec

