module CpuFreqsProc (main, getFreqsHandle) where
import Control.Monad (forever)
import Data.List (intercalate, nubBy)
import Data.Maybe (fromMaybe, listToMaybe)
import System.IO (hPutStr, hGetLine, hFlush)
import System.Process (system)
import System.Posix.IO (createPipe, fdToHandle)
import System.Posix.Process (forkProcess)
import Text.Regex.PCRE ((=~))
import Utils (chompFile)

main = do
  freqH <- getFreqsHandle
  forever $ do
    line <- hGetLine freqH
    putStrLn $ line

p x = do {putStrLn "----\n----"; putStrLn x; return ()}

getFreqsHandle = do
  (readFd, writeFd) <- createPipe
  readH <- fdToHandle readFd
  writeH <- fdToHandle writeFd

  forkProcess $ forever $ do
    cpuinfo <- chompFile "/proc/cpuinfo"
    let cpus = removeHTDupes $ getCpus cpuinfo
    let freqs = map snd cpus
    hPutStr writeH $ formatFreqs freqs
    hFlush writeH
    system "sleep 1"

  return readH

formatFreqs fs = (intercalate " " (map show fs)) ++ "\n"

toDouble = read :: String -> Double

getCpus :: String -> [(String, Integer)]
getCpus cpuinfo = map (\x -> (getCoreId x, getFreq x)) $ splitCpus cpuinfo

removeHTDupes :: [(String,Integer)] -> [(String,Integer)]
removeHTDupes = nubBy (\(id1,_) (id2,_) -> id1 == id2)

splitCpus cpuinfo = filter (/="") $ map unlines $ split (lines cpuinfo) [[]]

getCoreId cpu = coreId
  where coreId = fromMaybe ("-1") $ listToMaybe $ concat groupSets
        groupSets = map tail (cpu =~ p :: [[String]])
        p = "core id\\s*:\\s*(\\d+)"

getFreq cpu = round $ toDouble freq
  where freq = fromMaybe ("-1") $ listToMaybe $ concat groupSets
        groupSets = map tail (cpu =~ p :: [[String]])
        p = "cpu MHz\\s*:\\s*(\\d+\\.\\d+)"

split (ln:lns) (cpu:cpus) | ln == "" = split lns ([]:cpu:cpus)
split (ln:lns) (cpu:cpus) | otherwise = split lns ((ln:cpu):cpus)
split [] cpus = reverse cpus

