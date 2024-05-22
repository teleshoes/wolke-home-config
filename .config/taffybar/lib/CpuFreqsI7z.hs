module CpuFreqsI7z (getFreqsI7z) where
import Control.Monad (forever, void)
import Control.Concurrent (
  forkIO, myThreadId, killThread)
import Safe (headDef)
import System.Process (system, readProcess)
import System.Posix.Process (getProcessID)
import Utils (regexAllSubmatches, systemReadLines, readProc)
import System.Directory (setCurrentDirectory)

getFreqsI7z :: IO (IO [Int])
getFreqsI7z = do
  system "sudo i7z-kill"
  system "pkill -f 'tail -n 0 -F /tmp/i7z'"
  pid <- getProcessID
  let logFile = "/tmp/i7z_" ++ show pid ++ ".log"

  execAndThenDie $ i7zCmdArr logFile
  return $ readI7zLogFile logFile

i7zCmdArr log = ["sudo", "i7z", "--nogui", "-w", "a", "--logfile", log]

readI7zLogFile :: String -> IO [Int]
readI7zLogFile logFile = do
  line <- tailFile logFile
  return $ parseI7zLog line

execAndThenDie cmdArr = do
  parentThreadId <- myThreadId
  forkIO $ do
    setCurrentDirectory "/tmp"
    void $ readProc cmdArr
    killThread parentThreadId

tailFile :: String -> IO String
tailFile f = do fmap (headDef "") $ systemReadLines $ "tail -n 1 " ++ f ++ " 2>/dev/null"

numbers s = concat groupSets
  where groupSets = map tail (regexAllSubmatches p s)
        p = "(\\d+(?:\\.\\d+)?)"

toDouble = read :: String -> Double

--"bignumber Mhz Mhz ... " -> [Mhz]
parseI7zLog :: String -> [Int]
parseI7zLog line = filter (<10^9) $ map (round.toDouble) $ numbers line
