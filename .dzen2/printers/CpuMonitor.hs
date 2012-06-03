module CpuMonitor(main) where
import Control.Monad (forever)
import Control.Concurrent (
  Chan, forkIO, writeList2Chan, readChan, writeChan, newChan, threadDelay)
import Data.List (intercalate)
import Text.Regex.PCRE ((=~))
import Utils (height, lineBuffering, readProc,systemReadLines)
import PercentMonitor (percentMonitor)
import System.IO (hPutStrLn, stdout)

colors = reverse ["#0072b2", "#0091e5", "#00a2fe", "#002f3d", "#000000"]

main = do
  lineBuffering
  let (w, h) = (fromIntegral height, fromIntegral height)
  topLines <- systemReadLines (topCpuCmd 1)
  reader <- listToChan $ map (formatCpuPercent . cpuPercent . topMatch) topLines
  writer <- printChan stdout
  percentMonitor w h colors reader writer

topCpuCmd d = "top -b -p 1 -d " ++ show d ++ " | grep --line-buffered ^Cpu"

printChan outH = do
  chan <- newChan
  forkIO $ forever $ do
    line <- readChan chan
    hPutStrLn outH line
  return chan

listToChan :: [a] -> IO (Chan a)
listToChan xs = newChan >>= (\c -> forkIO (writeList2Chan c xs) >> return c)

formatCpuPercent = (intercalate " ")  . (map show)

cpuPercent (Just cpu) = [us, sy, ni, wa, idle]
  where (us, sy, ni, id, wa, hi, si, st) = cpu
        idle = 100.0 - us - sy - ni - wa
cpuPercent Nothing = take 5 $ 100.0:repeat 0

topMatch :: String -> Maybe (Float, Float, Float, Float,
                             Float, Float, Float, Float)
topMatch s = if isMatch then Just (f1, f2, f3, f4, f5, f6, f7, f8) else Nothing
  where regex = "^Cpu\\(s\\):" ++ bucketsRe ++ "$"
        buckets = ["us", "sy", "ni", "id", "wa", "hi", "si", "st"]
        bucketsRe = intercalate "," $ map ("\\s*(\\d+\\.\\d+)%" ++) buckets

        match = s =~ regex :: [[String]]
        isMatch = length match == 1
        [f1,f2,f3,f4,f5,f6,f7,f8] = map read $ drop 1 $ head match :: [Float]
