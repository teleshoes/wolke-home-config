module CpuFreqs (main) where
import CpuFreqsI7z (getFreqsHandle)
--import CpuFreqsProc (getFreqsHandle)
import TextRows(textRows)
import Data.List (intercalate)
import Control.Applicative ((<$>))
import System.IO (hGetLine, hFlush, stdout, Handle)
import Text.Regex.PCRE ((=~))
import Utils (posX)

main = do
  freqH <- getFreqsHandle
  cpuFreqLoop freqH []

cpuFreqLoop :: Handle -> [Int] -> IO ()
cpuFreqLoop freqH lengths = do
  freqs <- ints <$> hGetLine freqH
  let newLengths = take 10 $ (length freqs):lengths
  putStrLn $ formatFreqs freqs (maximum newLengths)
  hFlush stdout
  cpuFreqLoop freqH newLengths

formatFreqs freqs maxLen = intercalate (posX 4) $ formatCols formattedFreqs
  where formattedFreqs = take maxLen $ (map showFreq freqs) ++ (repeat "??")
        len = maxLen + (maxLen `mod` 2)

formatCols (f1:f2:fs) = textRows f1 f2 : formatCols fs
formatCols (f1:fs) = f1 : formatCols fs
formatCols _ = []

showFreq mhz = (if mhz < 1000 then "0" else "") ++ (show $ mhz `div` 100)

toInt = read :: String -> Integer
ints s = map toInt $ concat $ map tail (s =~ "(\\d+)" :: [[String]])

