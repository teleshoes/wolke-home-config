module CpuScaling(main) where
import Utils (fg, bg, padL, regexGroups,
              readInt, collectInts, chompFile, readProc)
import TextRows (textRows)

import Control.Monad (void)
import System.Process (system)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust, listToMaybe)

width = 2

tmpFile = "/tmp/cpu-scaling"
cpuDir = "/sys/devices/system/cpu"

main = do
  gov <- getCpuField "governor"
  minKHz <- getCpuFieldInt "min_freq"
  maxKHz <- getCpuFieldInt "max_freq"
  avail <- sort <$> getCpuFieldInts "available_frequencies"
  cur <- parseTmpFile avail <$> chompFile tmpFile
  (okGov, okMinKHz, okMaxKHz) <- check gov minKHz maxKHz avail cur
  putStrLn $ formatScaling okGov okMinKHz okMaxKHz avail

allSame [] = False
allSame [_] = True
allSame (x:y:xs) = x == y && allSame (y:xs)

getDevices :: String -> IO [String]
getDevices field = lines <$> readProc ["find", cpuDir, "-regex", regex]
  where regex = cpuDir ++ "/cpu[0-9]+/cpufreq/scaling_" ++ field

getCpuField field = do
  devices <- getDevices field
  vals <- mapM chompFile devices
  return $ if allSame vals then listToMaybe vals else Nothing

getCpuFieldInt f = readInt <$> fromMaybe "" <$> getCpuField f
getCpuFieldInts f = collectInts <$> fromMaybe "" <$> getCpuField f

setCpuField field val = do
  devices <- getDevices field
  mapM (writeFile val) devices

readTmp = collectInts <$> chompFile tmpFile

check gov minKHz maxKHz avail (curGov, curMin, curMax, curFreq) = do
  okGov <- checkMaybe gov "no governor!"
  okMin <- checkMaybe minKHz "no min freq!"
  okMax <- checkMaybe maxKHz "no max freq!"
  check (not $ null avail) "no available frequencies!"
  check (okGov == curGov) "mismatched governor!"
  check (okMin == curMin) "mismatched min freq!"
  check (okMax == curMax) "mismatched max freq!"
  return (okGov, okMin, okMax)
  where check cond str = if cond
                         then return ()
                         else rerun $ "Rerunning: " ++ str
        checkMaybe x str = do check (isJust x) str
                              return $ fromMaybe (error str) x
        rerun s = do
          print s
          readProc ["sudo", "cpu-set", curGov, show curMin, show curMax]
          error s

parseTmpFile avail s = parseGroups $ fromMaybe defaultTmp grps
  where re = ""
             ++ "governor=(.*)\\n?"
             ++ "min=(\\d*)\\n?"
             ++ "max=(\\d*)\\n?"
             ++ "freq=(\\d*)\\n?"
        grps = regexGroups re s
        defaultTmp = ["ondemand", show $ head avail, show $ last avail, ""]
        parseGroups [g,min,max,freq] = (g, toInt min, toInt max, toInt freq)
        toInt = fromMaybe 0 . readInt

formatScaling gov minKHz maxKHz avail = col $ textRows (pad top) (pad bot)
  where col = color minKHz maxKHz avail
        pad = padL '0' width . take width
        (top, bot) = (show $ minKHz `div` 10^5, show $ maxKHz `div` 10^5)

color min max avail
  | min == low && max == high = bg "blue"
  | min == low && max == low = bg "red" . fg "black"
  | min == high && max == high = bg "green". fg "black"
  | otherwise = bg "orange" . fg "black"
  where low = if null avail then 0 else head avail
        high = if null avail then 0 else last avail
