module Sep(sepW) where

import Graphics.UI.Gtk (
  toWidget, widgetShowAll,
  eventBoxNew, hBoxNew, containerAdd, widgetSetSizeRequest,
  Color(..), StateType(..), widgetModifyBg)

sepColor = Color 0 0 0
sepWidth = 2

sepW = do
  bgBox <- eventBoxNew
  widgetModifyBg bgBox StateNormal sepColor

  sizeBox <- hBoxNew False 0
  widgetSetSizeRequest sizeBox sepWidth (-1)

  containerAdd bgBox sizeBox
  widgetShowAll bgBox
  return $ toWidget bgBox
