module MemMonitor(main) where
import Control.Monad (forever)
import Control.Concurrent (forkIO, readChan, writeChan, newChan, threadDelay)
import Data.List (intercalate)
import Text.Regex.PCRE ((=~))
import Utils (height, lineBuffering, readProc)
import PercentMonitor (percentMonitor)
import System.IO (hPutStrLn, stdout)

colors = reverse ["#00b25b", "00e575", "#00fe81", "#a9f4c4", "#000000"]

main = do
  lineBuffering
  let (w, h) = (fromIntegral height, fromIntegral height)
  reader <- delayedChanReader perIo 1
  writer <- printChan stdout
  percentMonitor w h colors reader writer

perIo = fmap (formatMemPercent . memPercent . freeMatch) $ readProc ["free"]

printChan outH = do
  chan <- newChan
  forkIO $ forever $ do
    line <- readChan chan
    hPutStrLn outH line
  return chan

delayedChanReader getLine delay = do
  chan <- newChan
  forkIO $ forever $ do
    line <- getLine
    writeChan chan line
    threadDelay $ delay * 10^6
  return chan

formatMemPercent = (intercalate " ")  . (map show)

memPercent (Just mem) = map per [usedReal, shared, buffers, cached, free]
  where per x = 100.0 * fromIntegral x / fromIntegral total
        (total, used, free, shared, buffers, cached) = mem
        usedReal = used - shared - buffers - cached
memPercent Nothing = take 5 $ 100.0:repeat 0

freeMatch :: String -> Maybe (Int, Int, Int, Int, Int, Int)
freeMatch s = if isMatch then Just (i1, i2, i3, i4, i5, i6) else Nothing
  where regex = "^Mem:" ++ (concat $ replicate 6 "\\s*(\\d+)") ++ "$"
        match = s =~ regex :: [[String]]
        isMatch = length match == 1
        [i1,i2,i3,i4,i5,i6] = map read $ drop 1 $ head match
