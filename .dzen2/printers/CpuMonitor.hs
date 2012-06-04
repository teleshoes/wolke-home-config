module CpuMonitor(main) where
import Control.Monad (forever)
import Control.Concurrent (
  Chan, forkIO, writeList2Chan, readChan, writeChan, newChan, threadDelay)
import Data.List (intercalate)
import Text.Regex.PCRE ((=~))
import Utils (height, regexGroups, lineBuffering, systemReadLines)
import PercentMonitor (percentMonitor)
import System.IO (hPutStrLn, stdout)

colors = ["#0072b2", "#0091e5", "#00a2fe", "#002f3d", "#000000"]

main = do
  lineBuffering
  let (w, h) = (fromIntegral height, fromIntegral height)
  topLines <- systemReadLines $ topCpuCmd 1
  perChan <- listToChan $ map topToPercents topLines
  percentMonitor w h colors perChan

listToChan :: [a] -> IO (Chan a)
listToChan xs = newChan >>= (\c -> forkIO (writeList2Chan c xs) >> return c)

topCpuCmd d = "top -b -p 1 -d " ++ show d ++ " | grep --line-buffered ^Cpu"

topToPercents :: String -> [Float]
topToPercents line = maybe (take 5 $ 100.0:repeat 0) parseTop groups
  where regex = "^Cpu\\(s\\):" ++ cpuTsRe ++ "$"
        cpuTs = ["us", "sy", "ni", "id", "wa", "hi", "si", "st"]
        cpuTsRe = intercalate "," $ map ("\\s*(\\d+\\.\\d+)%" ++) cpuTs
        groups = fmap (map read) $ regexGroups regex line

parseTop [us, sy, ni, id, wa, hi, si, st] = [us, sy, ni, wa, idle]
  where idle = 100.0 - us - sy - ni - wa
