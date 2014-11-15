module Label(labelW, mainLabel) where
import Utils (defaultDelay)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Graphics.UI.Gtk (widgetShowAll)
import System.Taffybar.Widgets.PollingLabel (pollingLabelNew)

mainLabel printer = forever $ do
  msg <- printer
  print msg
  threadDelay $ round $ defaultDelay * 10^6

labelW printer = do
  w <- pollingLabelNew "---" defaultDelay printer
  widgetShowAll w
  return w
