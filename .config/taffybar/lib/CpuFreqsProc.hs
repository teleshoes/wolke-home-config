module CpuFreqsProc (getFreqsProc) where
import Data.List (nubBy)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Process (system)
import Utils (regexFirstGroup, chompFile)

getFreqsProc :: IO (IO [Int])
getFreqsProc = return $ fmap parseCpuInfo readCpuInfo
  where readCpuInfo = chompFile "/proc/cpuinfo"
        parseCpuInfo = map snd . removeHTDupes . getCpus

toDouble = read :: String -> Double

getCpus :: String -> [(String, Int)]
getCpus cpuinfo = map (\x -> (getCoreId x, getFreq x)) $ splitCpus cpuinfo

removeHTDupes :: [(String,Int)] -> [(String,Int)]
removeHTDupes = nubBy (\(id1,_) (id2,_) -> id1 == id2)

splitCpus cpuinfo = filter (/="") $ map unlines $ split (lines cpuinfo) [[]]

getCoreId cpu = fromMaybe ("-1") $ regexFirstGroup "core id\\s*:\\s*(\\d+)" cpu

getFreq cpu = round $ toDouble freq
  where freq = fromMaybe ("-1") (regexFirstGroup "cpu MHz\\s*:\\s*(\\d+\\.\\d+)" cpu)

split (ln:lns) (cpu:cpus) | ln == "" = split lns ([]:cpu:cpus)
split (ln:lns) (cpu:cpus) | otherwise = split lns ((ln:cpu):cpus)
split [] cpus = reverse cpus

