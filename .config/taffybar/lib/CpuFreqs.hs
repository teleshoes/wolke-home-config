module CpuFreqs (cpuFreqsW) where
import Label (labelW, mainLabel)

import CpuFreqsI7z (getFreqsI7z)
import CpuFreqsProc (getFreqsProc)
import Control.Concurrent (newMVar, modifyMVar)
import Control.Monad (forever)
import Data.List (intercalate)

main = mainLabel =<< cpuFreqsReader
cpuFreqsW = labelW =<< cpuFreqsReader

getFreqs :: IO (IO [Int])
getFreqs = getFreqsProc
--getFreqs = getFreqsI7z

cpuFreqsReader :: IO (IO String)
cpuFreqsReader = do
  maxLenVar <- newMVar 0
  getFreqsAct <- getFreqs
  return $ readCpuFreqs maxLenVar getFreqsAct

maxMVar mvar test = modifyMVar mvar maxTest
  where maxTest old = let new = max old test in return (new, new)

readCpuFreqs maxLenVar getFreqsAct = do
  freqs <- getFreqsAct
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
