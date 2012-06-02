module PingMonitor (main) where
import Utils (fg, lineBuffering, procSuccess)
import System.Environment (getEnv)
import Control.Concurrent (threadDelay)
import System.Environment.UTF8 (getArgs)

ping url timeout = procSuccess ["ping", url, "-c", "1", "-w", timeout]

main = do
  lineBuffering
  (url, display, timeout) <- fmap parseArgs getArgs
  putStrLn $ "?" ++ display
  pingMonitorLoop url display timeout ["/", "|"]

parseArgs (url:display:timeout:[]) = (url,display,timeout)
parseArgs _ = error "Usage: url display timeout"

pingMonitorLoop u d t = mapM (pingMonitor u d t) . cycle

pingMonitor url display timeout prefix = do
  isUp <- ping url timeout
  putStrLn $ fg (if isUp then "purple" else "red") (prefix ++ display)
  threadDelay $ (if isUp then 3 else 1) * 10^6
