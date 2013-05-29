module Sep(sepW) where
import Color (Color, widgetBgColorWrap)
import Width (widthBox)
import Graphics.UI.Gtk (Widget)

sepW :: Color -> Int -> IO Widget
sepW color width = widgetBgColorWrap color =<< widthBox width
