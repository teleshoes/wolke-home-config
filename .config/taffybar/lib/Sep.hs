module Sep(sepW) where

import Graphics.UI.Gtk (
  frameNew, toWidget, widgetShowAll,
  Color(..), StateType(..), widgetModifyBg)

sepColor = Color 0 0 0

sepW = do
  f <- frameNew
  widgetModifyBg f StateNormal sepColor
  widgetShowAll f
  return $ toWidget f
