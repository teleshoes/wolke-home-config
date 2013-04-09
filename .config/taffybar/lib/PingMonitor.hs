module PingMonitor (pingMonitorW) where
import Utils (defaultDelay, fg, procSuccess)
import Widgets (label)

import System.Environment (getEnv)
import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import System.Environment.UTF8 (getArgs)

isPingable url timeout = procSuccess ["ping", url, "-c", "1", "-w", show timeout]

pingMonitorW url display = do
  chan <- newChan
  writeChan chan $ "?" ++ display
  let prefixes = cycle [fg "purple" "/", fg "green" "|"]
  forkIO $ mapM_ (ping chan url display defaultDelay) prefixes
  lbl <- label $ readChan chan
  return lbl

ping chan url display timeout prefix = do
  isUp <- isPingable url timeout
  let (color, wait) = if isUp then ("yellow", 3) else ("red", 1)
  writeChan chan $ prefix ++ fg color display
  threadDelay $ wait * 10^6
