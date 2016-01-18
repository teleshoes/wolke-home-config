module NetStats(netStatsW) where
import Utils (nanoTime, fg, chompFile, regexMatch, regexGroups)
import Label (labelW, mainLabel)

import Control.Concurrent (threadDelay, forkIO, readChan, writeChan, newChan)
import Control.Monad (forever)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.List (minimumBy, maximumBy)
import Text.Printf (printf)

main = mainLabel =<< netStatsReader
netStatsW = labelW =<< netStatsReader

netStatsReader :: IO (IO String)
netStatsReader = do
  chan <- newChan
  forkIO $ scanLoop chan []
  return $ readChan chan

ignoredInterfacesRegex = "^(lo|tun\\d+)$"

procFile = "/proc/net/dev"

data NetScan = NetScan { scanTime :: Integer, devs :: [NetDev] } deriving Show

data NetDev = NetDev { interface :: String
                     , receive :: NetStats
                     , transmit :: NetStats
                     } deriving Show

data NetStats = NetStats { bytes :: Integer
                         , packets :: Integer
                         , errs :: Integer
                         , drop :: Integer
                         , fifo :: Integer
                         , frame :: Integer
                         , compressed :: Integer
                         , multicast :: Integer
                         } deriving Show

netdev :: [String] -> NetDev
netdev (interface:stats) = packDev interface $ splitAt 8 (map read stats)
  where
    packDev interface (rxStats,txStats) =
      NetDev interface (packStats rxStats) (packStats txStats)
    packStats [bytes,packets,errs,drop,fifo,frame,compressed,multicast] =
      NetStats bytes packets errs drop fifo frame compressed multicast

scanLoop chan scans = do
  (oldest, newest, updatedScans) <- updateScans scans
  writeChan chan $ format oldest newest
  threadDelay $ 1*10^6
  scanLoop chan updatedScans

showBytes bytes = fg (chooseColor byteColors) (unit (bytes/1024) (tail units))
  where
    numLen = 5
    unit :: Double -> [String] -> String
    unit x (u:[]) = fmt x ++ u
    unit x (u:us) | length (fmt x) > numLen = unit (x/1024) us
                  | otherwise = fmt x ++ u
    fmt n = printf ("%" ++ show numLen ++ ".1f") n :: String
    units = ["B", "K", "M", "G", "T", "P", "E", "Z", "Y"]
    byteColors = zip (map (*1024) [1, 8, 16, 128, 512, 1024, 4096])
                   ["black", "gray", "blue", "purple", "green", "white", "red"]
    chooseColor ((b, c):bcs) | bytes > b && length bcs > 0 = chooseColor bcs
                             | otherwise = c

format scanInitial scanFinal = dn ++ "\n" ++ up
  where
    dn = showBytes $ if elapsedSex == 0 then 0 else rxBytes / elapsedSex
    up = showBytes $ if elapsedSex == 0 then 0 else txBytes / elapsedSex
    rxBytes = fromIntegral $ statDiff receive bytes scanFinal scanInitial
    txBytes = fromIntegral $ statDiff transmit bytes scanFinal scanInitial
    elapsedSex = fromIntegral (scanTime scanFinal - scanTime scanInitial) / 10^9

statDiff rxTx stat f i = (totalStat rxTx stat f) - (totalStat rxTx stat i)
totalStat rxTx stat scan = sum $ map (stat.rxTx) $ devs scan

updateScans :: [NetScan] -> IO (NetScan, NetScan, [NetScan])
updateScans scans = do
  latestScan <- netScan
  let updatedScans = latestScan : (filterScans (scanTime latestScan) 5 scans)
  let oldest = minimumBy (comparing scanTime) updatedScans
  let newest = maximumBy (comparing scanTime) updatedScans
  return $ (oldest, newest, updatedScans)

filterScans nanoTime secondsAgo scans = filter ok scans
  where 
    targetTime = nanoTime - (secondsAgo * 10^9)
    ok scan = scanTime scan >= targetTime

netScan :: IO (NetScan)
netScan = do
  proc <- chompFile procFile
  time <- nanoTime
  return $ NetScan time $ parseProcNetDev proc

isIgnored NetDev{interface=iface} = regexMatch ignoredInterfacesRegex iface

parseProcNetDev :: String -> [NetDev]
parseProcNetDev proc = filter (not.isIgnored) $ map netdev groups
  where groups = catMaybes $ map (regexGroups re) $ lines proc
        re = "([a-z0-9]+):" ++ (concat $ replicate 16 "\\s*(\\d+)")
