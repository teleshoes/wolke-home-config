module Widgets(label) where
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)
import Graphics.UI.Gtk (widgetShowAll)

label interval printer = do
  w <- pollingLabelNew "---" interval printer
  widgetShowAll w
  return w
