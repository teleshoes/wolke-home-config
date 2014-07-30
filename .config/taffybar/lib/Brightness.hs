module Brightness (brightnessW) where
import PercentBarWidget (percentBarWidgetW, percentBarConfig)
import Color as C
import System.Environment (getEnv)
import System.Process(system)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Utils (readDouble, readProc)

colors = map C.rgb $ [C.Black, C.Gray] ++ take 10 (cycle [C.Blue, C.Orange])

brightnessW = percentBarWidgetW percentBarConfig 1 readBrightnessBar

lastBrightness = do
  home <- getEnv "HOME"
  system $ home ++ "/bin/brightness last > /dev/null"

readBrightnessBar = do
  system "$HOME/bin/brightness last > /dev/null"
  p <- getBrightness
  return (p, colors)

getBrightness = fmap parse $ readProc ["brightness"]
parse b = (fromMaybe 300.0 $ readDouble b) / 100.0
