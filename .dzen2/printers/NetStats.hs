module NetStats(main) where
import System.CPUTime (getCPUTime)
import Text.Regex.PCRE ((=~))
import Control.Monad (forever)
import System.Posix.Clock (timeSpecToInt64, monotonicClock, getClockTime)
import Control.Concurrent (threadDelay)
import Data.Int (Int64)
import Data.Ord (comparing)
import Data.List (minimumBy, maximumBy)
import Text.Printf (printf)
import TextRows (textRows)
import System.IO (hFlush, stdout)
import Utils (fg)

ignoredInterfacesRegex = "(lo|tun\\d+)"

procFile = "/proc/net/dev"

data NetScan = NetScan { scanTime :: Int64, devs :: [NetDev] } deriving Show

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

nanoTime = timeSpecToInt64 `fmap` (getClockTime monotonicClock)

main = scanLoop []

scanLoop scans = do
  (oldest, newest, updatedScans) <- updateScans scans
  putStrLn $ format oldest newest
  hFlush stdout
  threadDelay 1000000
  scanLoop updatedScans

showBytes bytes = fg (chooseColor byteColors) (unit (bytes/1024) units)
  where
    unit :: Double -> [String] -> String
    unit x (u:us) | x >= 1000 && length us > 0 = unit (x/1024) us
                  | otherwise = printf "%6.1f" x ++ u
    units = ["K", "M", "G", "T", "P", "E", "Z", "Y"]
    byteColors = zip
                   (map (*1024) [1, 8, 16, 128, 512, 1024, 4096])
                   ["black", "gray", "blue", "purple", "green", "white", "red"]
    chooseColor ((b, c):bcs) | bytes > b && length bcs > 0 = chooseColor bcs
                             | otherwise = c

format scanInitial scanFinal = textRows dn up
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
  proc <- readFile procFile
  time <- nanoTime
  return $ NetScan time $ parseProcNetDev proc

isIgnored NetDev{interface=iface} = isMatch iface ignoredInterfacesRegex

parseProcNetDev :: String -> [NetDev]
parseProcNetDev proc = filter (not.isIgnored) $ map netdev okGroups
  where
    groups = map (\line -> getMatches line re) $ lines proc
    okGroups = filter ((==17).length) groups
    re = "([a-z0-9]+):" ++ (concat $ replicate 16 "\\s*(\\d+)")

isMatch a re = a =~ re :: Bool
getMatches a re = concatMap tail groups
  where groups = a =~ re :: [[String]]
