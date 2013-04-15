module TPBattStat (tpBattStatW) where
import Widgets (label)
import JsonWidget (jsonWidgetNew)
import Utils (defaultDelay, procToChan)
import Control.Concurrent (readChan)

tpBattStatCmd h = [ "/usr/lib/tpbattstat-applet/tpbattstat.py"
                  , "--json"
                  , show $ floor $ defaultDelay * 1000
                  , show h ++ "x" ++ show h
                  ]

tpBattStatW h = do
  chan <- procToChan $ tpBattStatCmd h
  widget <- jsonWidgetNew $ readChan chan
  return widget
