module Label(labelW, labelDelayW, mainLabel) where
import Utils (defaultDelay)

import Data.Text (Text, pack)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)

import GI.Gtk.Objects.Widget (Widget)

mainLabel printer = forever $ do
  msg <- printer
  print msg
  threadDelay $ round $ defaultDelay * 10^6

labelW :: (IO String) -> IO Widget
labelW printer = labelDelayW defaultDelay printer

labelDelayW :: Double -> (IO String) -> IO Widget
labelDelayW delay printer = pollingLabelNew delay $ fmap pack printer
