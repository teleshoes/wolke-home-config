module TPBattStat (tpBattStatW) where
import JsonWidget (jsonWidgetNew)
import Utils (defaultDelay, procToChan)
import Control.Concurrent (readChan)
import Data.Maybe (fromMaybe, listToMaybe)

availableIconSizes = [18, 20, 24, 36, 38, 40, 48, 50, 64] :: [Int]

tpBattStatCmd h = [ "/usr/lib/tpbattstat-applet/tpbattstat.py"
                  , "--json"
                  , show $ floor $ defaultDelay * 1000
                  , sizeFmt
                  ]
  where okSizes = filter (<=h) availableIconSizes
        smallestSize = minimum availableIconSizes
        size = maximum (smallestSize:okSizes)
        sizeFmt = show size ++ "x" ++ show size

tpBattStatW h = do
  chan <- procToChan $ tpBattStatCmd h
  widget <- jsonWidgetNew $ readChan chan
  return widget
