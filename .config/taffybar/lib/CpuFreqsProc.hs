module CpuFreqsProc (getFreqsChanProc) where
import Control.Concurrent (Chan)
import Data.List (nubBy)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Process (system)
import Utils (regexAllSubmatches, chompFile, actToChanDelay)

getFreqsChanProc :: IO (Chan [Int])
getFreqsChanProc = actToChanDelay (10^6) (fmap parseCpuInfo readCpuInfo)
  where readCpuInfo = chompFile "/proc/cpuinfo"
        parseCpuInfo = map snd . removeHTDupes . getCpus

toDouble = read :: String -> Double

getCpus :: String -> [(String, Int)]
getCpus cpuinfo = map (\x -> (getCoreId x, getFreq x)) $ splitCpus cpuinfo

removeHTDupes :: [(String,Int)] -> [(String,Int)]
removeHTDupes = nubBy (\(id1,_) (id2,_) -> id1 == id2)

splitCpus cpuinfo = filter (/="") $ map unlines $ split (lines cpuinfo) [[]]

getCoreId cpu = coreId
  where coreId = fromMaybe ("-1") $ listToMaybe $ concat groupSets
        groupSets = map tail (regexAllSubmatches p cpu)
        p = "core id\\s*:\\s*(\\d+)"

getFreq cpu = round $ toDouble freq
  where freq = fromMaybe ("-1") $ listToMaybe $ concat groupSets
        groupSets = map tail (regexAllSubmatches p cpu)
        p = "cpu MHz\\s*:\\s*(\\d+\\.\\d+)"

split (ln:lns) (cpu:cpus) | ln == "" = split lns ([]:cpu:cpus)
split (ln:lns) (cpu:cpus) | otherwise = split lns ((ln:cpu):cpus)
split [] cpus = reverse cpus

