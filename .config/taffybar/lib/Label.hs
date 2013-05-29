module Label(labelW) where
import Utils (defaultDelay)
import Graphics.UI.Gtk (widgetShowAll)
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)

labelW printer = do
  w <- pollingLabelNew "---" defaultDelay printer
  widgetShowAll w
  return w
