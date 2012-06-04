module MemMonitor(main) where
import Control.Monad (forever)
import Control.Concurrent (Chan, forkIO, readChan, writeChan, newChan, threadDelay)
import Data.List (intercalate)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Text.Regex.PCRE ((=~))
import Utils (height, lineBuffering, readProc, regexGroups)
import PercentMonitor (percentMonitor)
import System.IO (Handle, hPutStrLn, stdout)

colors = ["#00b25b", "00e575", "#00fe81", "#a9f4c4", "#000000"]

main = do
  lineBuffering
  let (w, h) = (fromIntegral height, fromIntegral height)
  perChan <- delayedChanReader (fmap freeToPercents $ readProc ["free"]) 1
  percentMonitor w h colors perChan

delayedChanReader :: IO a -> Float -> IO (Chan a)
delayedChanReader ioA delay = do
  chan <- newChan
  forkIO $ forever $ do
    a <- ioA
    writeChan chan a
    threadDelay $ round (delay * 10^6)
  return chan

freeToPercents :: String -> [Float]
freeToPercents line = maybe (take 5 $ 100.0:repeat 0) parseFree groups
  where regex = "^Mem:" ++ (concat $ replicate 6 "\\s*(\\d+)") ++ "$"
        groups = fmap (map read) $ regexGroups regex line

parseFree [total, used, free, shared, buffers, cached] = pers
  where percent n = 100.0 * fromIntegral n / fromIntegral total
        pers = map percent [usedReal, shared, buffers, cached, free]
        usedReal = used - shared - buffers - cached

