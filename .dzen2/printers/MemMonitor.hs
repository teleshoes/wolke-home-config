module MemMonitor(main) where
import Utils (height, lineBuffering, readProc, regexGroups, actToChanDelay)
import PercentMonitor (percentMonitor)

colors = ["#00b25b", "00e575", "#00fe81", "#a9f4c4", "#000000"]

main = do
  lineBuffering
  let (w, h) = (fromIntegral height, fromIntegral height)
  perChan <- actToChanDelay (10^6) (fmap freeToPercents $ readProc ["free"])
  percentMonitor w h colors perChan

freeToPercents :: String -> [Float]
freeToPercents line = maybe (take 5 $ 100.0:repeat 0) parseFree groups
  where regex = "^Mem:" ++ (concat $ replicate 6 "\\s*(\\d+)") ++ "$"
        groups = fmap (map read) $ regexGroups regex line

parseFree [total, used, free, shared, buffers, cached] = pers
  where percent n = 100.0 * fromIntegral n / fromIntegral total
        pers = map percent [usedReal, shared, buffers, cached, free]
        usedReal = used - shared - buffers - cached
