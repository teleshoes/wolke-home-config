module Color(
  Color(..), rgb, rgba, gtkColor, hexColor,
  widgetBgColor, widgetBgColorWrap
) where

import Numeric (showHex)
import qualified Graphics.UI.Gtk as Gtk (Color(..))
import Graphics.UI.Gtk (
  StateType(..), widgetModifyBg,
  containerAdd, eventBoxNew, toWidget, widgetShowAll)

data Color = Black
           | White
           | Gray
           | Red
           | Green
           | Blue
           | Yellow
           | Purple
           | Cyan
           | Orange
           | RGB (Double, Double, Double)
  deriving (Show)

rgb c = case c of
          Black  -> (0.0, 0.0, 0.0)
          White  -> (1.0, 1.0, 1.0)
          Gray   -> (0.5, 0.5, 0.5)
          Red    -> (1.0, 0.0, 0.0)
          Green  -> (0.0, 1.0, 0.0)
          Blue   -> (0.0, 0.0, 1.0)
          Yellow -> (1.0, 1.0, 0.0)
          Purple -> (1.0, 0.0, 1.0)
          Cyan   -> (0.0, 1.0, 1.0)
          Orange -> (1.0, 0.5, 0.0)
          RGB(r,g,b) -> (r,g,b)

rgba c a = (r,g,b,a)
  where (r,g,b) = rgb c

gtkColor c = Gtk.Color (gtk r) (gtk g) (gtk b)
  where (r,g,b) = rgb c
        gtk = round . (65535*)

byte b = (if b < 16 then "0" else "") ++ showHex b ""

hexColor c = "#" ++ hex r ++ hex g ++ hex b
  where (r,g,b) = rgb c
        hex = byte . round . (255*)

widgetBgColor c w = widgetModifyBg w StateNormal (gtkColor c)

widgetBgColorWrap c w = do
  ebox <- eventBoxNew
  widgetBgColor c ebox
  containerAdd ebox w
  widgetShowAll ebox
  return $ toWidget ebox
