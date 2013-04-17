module Sep(sepW) where
import Color (Color, widgetBgColorWrap)
import Graphics.UI.Gtk (Widget, hBoxNew, widgetSetSizeRequest)

sepW :: Color -> Int -> IO Widget
sepW color width = do
  hbox <- hBoxNew False 0
  widgetSetSizeRequest hbox width (-1)
  widgetBgColorWrap color hbox
