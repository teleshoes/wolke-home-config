module TPBattStat (tpBattStatW) where
import Widgets (label)
import JsonWidget (jsonWidgetNew)
import Utils (procToChan)
import Control.Concurrent (readChan)

delayMillis = 1000

tpBattStatCmd = [ "/usr/lib/tpbattstat-applet/tpbattstat.py"
                , "--json"
                , show $ floor $ delayMillis]

tpBattStatW = do
  chan <- procToChan tpBattStatCmd
  widget <- jsonWidgetNew $ readChan chan
  return widget
