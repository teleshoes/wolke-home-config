module CpuFreqsI7z (main, getFreqsHandle) where
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Maybe (listToMaybe, fromMaybe)
import System.IO (hPutStr, hGetLine, hClose, Handle, stdin, stdout, stderr, hFlush)
import System.Process (system, runInteractiveProcess)
import System.Posix.IO (createPipe, fdToHandle)
import System.Posix.Process (forkProcess, executeFile, getProcessID)
import Text.Regex.PCRE ((=~))

main = do
  freqH <- getFreqsHandle
  forever $ do
    line <- hGetLine freqH
    putStrLn $ line

getFreqsHandle = do
  system "sudo i7z-kill"
  pid <- getProcessID
  let logfile = "/tmp/i7z_" ++ show pid ++ ".log"

  forkExec $ i7zCmdArr logfile
  logFH <- tailFile logfile

  (readFd, writeFd) <- createPipe
  readH <- fdToHandle readFd
  writeH <- fdToHandle writeFd

  forkProcess $ forever $ do
    line <- hGetLine logFH
    let freqs = parseI7zLog line
    hPutStr writeH $ formatFreqs freqs
    hFlush writeH

  return readH

sh cmd = ["sh", "-c", intercalate " " cmd]

i7zCmdArr log = sh
  [ "sudo"
  , "i7z"
  , "--nogui"
  , "-w", "a"
  , "--logfile", log
  ]

forkExec (cmd:args) = forkProcess $ do
  hClose stdin
  hClose stdout
  hClose stderr
  executeFile cmd True args Nothing

tailFile f = do
  let args = [ "-F", f
             , "-n", "0"
             ]
  (_,fh,_,_) <- runInteractiveProcess "tail" args Nothing Nothing
  return fh

formatFreqs fs = (intercalate " " (map show fs)) ++ "\n"

numbers s = concat groupSets
  where groupSets = map tail (s =~ p :: [[String]])
        p = "(\\d+(?:\\.\\d+)?)"

toDouble = read :: String -> Double

--"bignumber Mhz Mhz ... " -> [Mhz]
parseI7zLog :: String -> [Integer]
parseI7zLog line = filter (<10^9) $ map (round.toDouble) $ numbers line

