module CpuFreqs (main) where
import CpuFreqsI7z (getFreqsChanI7z)
import CpuFreqsProc (getFreqsChanProc)
import TextRows(textRows)
import Control.Concurrent (Chan, readChan)
import Data.List (intercalate)
import Utils (posX, lineBuffering)

main = do
  lineBuffering
  freqsChan <- getFreqsChanI7z
  cpuFreqLoop freqsChan []

cpuFreqLoop :: Chan [Int] -> [Int] -> IO ()
cpuFreqLoop freqsChan lengths = do
  freqs <- readChan freqsChan
  let newLengths = take 10 $ (length freqs):lengths
  putStrLn $ formatFreqs freqs (maximum newLengths)
  cpuFreqLoop freqsChan newLengths

formatFreqs freqs maxLen = intercalate (posX 4) $ formatCols formattedFreqs
  where formattedFreqs = take maxLen $ (map showFreq freqs) ++ (repeat "??")
        len = maxLen + (maxLen `mod` 2)

formatCols (f1:f2:fs) = textRows f1 f2 : formatCols fs
formatCols (f1:fs) = f1 : formatCols fs
formatCols _ = []

showFreq mhz = (if mhz < 1000 then "0" else "") ++ (show $ mhz `div` 100)
