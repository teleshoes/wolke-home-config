module Brightness (brightnessW) where
import PercentBarWidget (
  percentBarWidgetW, percentBarConfig, colorMap, cycleColors)
import System.Environment (getEnv)
import System.Process(system)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Utils (readDouble, readProc)

brightnessW = percentBarWidgetW percentBarConfig 1 readBrightnessBar

lastBrightness = do
  home <- getEnv "HOME"
  system $ home ++ "/bin/brightness last > /dev/null"

readBrightnessBar = do
  let colors = map colorMap $ ["black", "gray"] ++ repeat "orange"
  system "$HOME/bin/brightness last > /dev/null"
  p <- getBrightness
  let (bg, fg) = cycleColors colors p
  return (fg, bg, p)

getBrightness = fmap parse $ readProc ["xbacklight", "-get"]
parse b = (fromMaybe 300.0 $ readDouble b) / 100.0
