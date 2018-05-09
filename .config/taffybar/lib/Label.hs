module Label(labelW, labelDefaultW, mainLabel) where
import Utils (defaultDelay)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Graphics.UI.Gtk (widgetShowAll)
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)

mainLabel printer = forever $ do
  msg <- printer
  print msg
  threadDelay $ round $ defaultDelay * 10^6

labelW printer = labelDefaultW "---" printer

labelDefaultW defaultStr printer = do
  w <- pollingLabelNew defaultStr defaultDelay printer
  widgetShowAll w
  return w
