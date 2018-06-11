module Brightness (brightnessW) where
import PercentBarWidget (percentBarWidgetW, mainPercentBarWidget)
import Color as C
import System.Environment (getEnv)
import System.Process(system)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Utils (readDouble, readProc)

main = mainPercentBarWidget 1 readBrightnessBar
brightnessW = percentBarWidgetW 1 readBrightnessBar

colors = map C.rgb $ [C.Black, C.Gray] ++ take 10 (cycle [C.Blue, C.Orange])

lastBrightness = do
  system $ "brightness last > /dev/null"

readBrightnessBar = do
  system "brightness last > /dev/null"
  p <- getBrightness
  return (p, colors)

getBrightness = fmap parse $ readProc ["brightness"]
parse b = (fromMaybe 300.0 $ readDouble b) / 100.0
