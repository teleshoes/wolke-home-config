module Label(lbl) where
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk (widgetShowAll)

lbl interval printer = do
  w <- pollingLabelNew "---" interval printer
  widgetShowAll w
  return w
