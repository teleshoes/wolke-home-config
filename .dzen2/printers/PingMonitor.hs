module PingMonitor (main) where
import Utils (fg, lineBuffering, procSuccess)
import System.Environment (getEnv)
import Control.Concurrent (threadDelay)
import System.Environment.UTF8 (getArgs)

isUp url timeout = procSuccess ["ping", url, "-c", "1", "-w", timeout]

main = do
  lineBuffering
  (url, display, timeout) <- fmap parseArgs getArgs
  pingMonitorLoop url display timeout True

pingMonitorLoop url display timeout toggle = do
  up <- isUp url timeout
  let msg = (if toggle then "/" else "|") ++ display
  putStrLn $ fg (if up then "purple" else "red") msg
  threadDelay $ (if up then 3 else 1) * 10^6
  pingMonitorLoop url display timeout (not toggle)

parseArgs (url:display:timeout:[]) = (url,display,timeout)
parseArgs _ = error "Usage: url display timeout"
