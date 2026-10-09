module CpuFreqsProc (getFreqsProc) where
import Data.List (nubBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.IO (readFile)
import Utils (regexFirstGroup)

getFreqsProc :: IO (IO [Int])
getFreqsProc = return $ fmap parseAllCpuInfo readAllCpuInfo

readAllCpuInfo :: IO String
readAllCpuInfo = readFile "/proc/cpuinfo"

parseAllCpuInfo :: String -> [Int]
parseAllCpuInfo allCpusInfoStr = map snd $ uniqCoreIdCpus
  where cpuStrs = splitOn "\n\n" allCpusInfoStr
        allCpus = filter ((>=0).snd) $ map parseCpu cpuStrs
        uniqCoreIdCpus = nubBy (\cpu1 cpu2 -> fst cpu1 == fst cpu2) allCpus

parseCpu :: String -> (Int, Int)
parseCpu cpuStr = (getCoreId cpuStr, getFreqMHz cpuStr)

removeHTDupes :: [(Int,Int)] -> [(Int,Int)]
removeHTDupes = nubBy (\(id1,_) (id2,_) -> id1 == id2)

getCoreId :: String -> Int
getCoreId cpuStr = read $ fromMaybe ("-1") $ regexFirstGroup "core id\\s*:\\s*(\\d+)" cpuStr

getFreqMHz :: String -> Int
getFreqMHz cpuStr = round $ toDouble freq
  where freq = fromMaybe ("-1") $ regexFirstGroup "cpu MHz\\s*:\\s*(\\d+\\.\\d+)" cpuStr
        toDouble = read :: String -> Double
