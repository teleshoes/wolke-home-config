module PingMonitor (pingMonitorLabel) where
import Label(lbl)
import Utils (fg, procSuccess)
import System.Environment (getEnv)
import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import System.Environment.UTF8 (getArgs)

ping url timeout = procSuccess ["ping", url, "-c", "1", "-w", show timeout]

pingMonitorLabel url display timeout = do
  chan <- newChan
  writeChan chan $ "?" ++ display
  let prefixes = cycle [fg "purple" "/", fg "green" "|"]
  forkIO $ mapM_ (pingMonitor chan url display timeout) prefixes
  lbl timeout $ readChan chan

pingMonitor chan url display timeout prefix = do
  isUp <- ping url timeout
  let (color, wait) = if isUp then ("yellow", 3) else ("red", 1)
  writeChan chan $ prefix ++ fg color display
  threadDelay $ wait * 10^6
