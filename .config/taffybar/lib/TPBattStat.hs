module TPBattStat (tpBattStatW) where
import Label (mainLabel)
import JsonWidget (jsonWidgetNew)
import Utils (defaultDelay, procToChan)
import Control.Concurrent (readChan)
import Data.Maybe (fromMaybe, listToMaybe)

main = mainLabel =<< tpBattStatReader 30

tpBattStatW h = jsonWidgetNew =<< tpBattStatReader h

tpBattStatReader :: Int -> IO (IO String)
tpBattStatReader h = fmap readChan $ procToChan $ tpBattStatCmd h

availableIconSizes = [18, 20, 24, 36, 38, 40, 48, 50, 64] :: [Int]

tpBattStatCmd :: Int -> [String]
tpBattStatCmd h = [ "/usr/lib/tpbattstat-applet/tpbattstat.py"
                  , "--json"
                  , show $ floor $ defaultDelay * 1000
                  , sizeFmt
                  ]
  where okSizes = filter (<=h) availableIconSizes
        smallestSize = minimum availableIconSizes
        size = maximum (smallestSize:okSizes)
        sizeFmt = show size ++ "x" ++ show size
