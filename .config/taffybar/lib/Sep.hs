module Sep(sepW) where
import Color (Color, gtkColor)
import Graphics.UI.Gtk (
  Widget, toWidget, widgetShowAll,
  eventBoxNew, hBoxNew, containerAdd, widgetSetSizeRequest,
  StateType(..), widgetModifyBg)

sepW :: Color -> Int -> IO Widget
sepW color width = do
  bgBox <- eventBoxNew
  widgetModifyBg bgBox StateNormal (gtkColor color)

  sizeBox <- hBoxNew False 0
  widgetSetSizeRequest sizeBox width (-1)

  containerAdd bgBox sizeBox
  widgetShowAll bgBox
  return $ toWidget bgBox
