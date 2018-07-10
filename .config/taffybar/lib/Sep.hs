module Sep(sepW) where
import Width (widthBox)

import Data.Text (pack)
import GI.Gtk.Objects.Widget (Widget, toWidget)
import System.Taffybar.Widget.Util (widgetSetClassGI)

sepW :: Int -> IO Widget
sepW width = do
  box <- widthBox width
  w <- toWidget box
  widgetSetClassGI w $ pack "Sep"
