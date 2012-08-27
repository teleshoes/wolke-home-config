module CpuScalingGov(main) where
import Utils (fg, bg, padL, readInt, collectInts, chompFile, runProc, readProc)
import TextRows (textRows)

import Control.Monad (void)
import System.Process (system)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust, listToMaybe)

width = 2

tmpFile = "/tmp/cpufreq"
cpuDir = "/sys/devices/system/cpu"

allSame [] = False
allSame [_] = True
allSame (x:y:xs) = x == y && allSame (y:xs)

getCpuField f = do
  let regex = cpuDir ++ "/cpu[0-9]+/cpufreq/scaling_" ++ f
  devices <- fmap lines $ readProc ["find", cpuDir, "-regex", regex]
  vals <- mapM chompFile devices
  return $ if allSame vals then listToMaybe vals else Nothing

getCpuFieldInt f = readInt <$> fromMaybe "" <$> getCpuField f
getCpuFieldInts f = collectInts <$> fromMaybe "" <$> getCpuField f

readTmp = collectInts <$> chompFile tmpFile

die s = do print s; error s
maybeDie s x = if not $ isJust x then die s else return $ fromMaybe (error s) x
check gov minKHz maxKHz avail = do
  maybeDie "No governor!" gov
  maybeDie "No min freq!" minKHz
  maybeDie "No max freq!" maxKHz
  if null avail then die "No available frequences!" else return ()

main = do
  gov <- getCpuField "governor"
  minKHz <- getCpuFieldInt "min_freq"
  maxKHz <- getCpuFieldInt "max_freq"
  avail <- sort <$> getCpuFieldInts "available_frequencies"
  check gov minKHz maxKHz avail
  putStrLn $ formatScaling (fm "" gov) (fm 0 minKHz) (fm 0 maxKHz) avail
  where fm = fromMaybe

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
