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

pingMonitorLoop u d t = mapM_ (pingMonitor u d t) . cycle

pingMonitor url display timeout prefix = do
  isUp <- ping url timeout
  let (color, wait) = if isUp then ("yellow", 3) else ("red", 1)
  putStrLn $ fg color (prefix ++ display)
  threadDelay $ wait * 10^6
