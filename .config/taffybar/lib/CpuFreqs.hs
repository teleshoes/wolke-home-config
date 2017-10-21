module CpuFreqs (cpuFreqsW) where
import Label (labelDefaultW, mainLabel)

import CpuFreqsI7z (getFreqsChanI7z)
import CpuFreqsProc (getFreqsChanProc)
import Control.Concurrent (newMVar, modifyMVar, readChan)
import Control.Monad (forever)
import Data.List (intercalate)

main = mainLabel =<< cpuFreqsReader
cpuFreqsW = labelDefaultW "--" =<< cpuFreqsReader

getFreqsChan = getFreqsChanProc

cpuFreqsReader :: IO (IO String)
cpuFreqsReader = do
  freqsChan <- getFreqsChan
  maxLenVar <- newMVar 0
  return $ readCpuFreqs freqsChan maxLenVar

maxMVar mvar test = modifyMVar mvar maxTest
  where maxTest old = let new = max old test in return (new, new)

readCpuFreqs freqsChan maxLenVar = do
  freqs <- readChan freqsChan
  maxLen <- maxMVar maxLenVar $ length freqs
  return $ formatFreqs freqs maxLen

formatFreqs freqs maxLen = formatRows 2 formattedFreqs
  where formattedFreqs = take maxLen $ (map showFreq freqs) ++ (repeat "??")

segs xs n = if n == 0 then [] else cs xs []
  where size = length xs `div` n
        cs xs xss | length xss == n-1 = reverse (xs:xss)
                  | otherwise = cs (drop size xs) (take size xs:xss)

formatRows rows xs = intercalate "\n" $ map (intercalate " ") $ segs xs rows

showFreq mhz = (if mhz < 1000 then "0" else "") ++ (show $ mhz `div` 100)
