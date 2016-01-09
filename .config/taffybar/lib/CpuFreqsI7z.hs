module CpuFreqsI7z (getFreqsChanI7z) where
import Control.Monad (forever, void)
import Control.Concurrent (
  forkIO, myThreadId, killThread, Chan)
import System.Process (system, readProcess)
import System.Posix.Process (getProcessID)
import Utils (regexAllSubmatches, systemReadLines, readProc, listToChan)
import System.Directory (setCurrentDirectory)

getFreqsChanI7z :: IO (Chan [Int])
getFreqsChanI7z = do
  system "sudo i7z-kill"
  system "pkill -f 'tail -n 0 -F /tmp/i7z'"
  pid <- getProcessID
  let logFile = "/tmp/i7z_" ++ show pid ++ ".log"

  execAndThenDie $ i7zCmdArr logFile
  i7zLogLines <- fmap (map parseI7zLog) $ tailFile logFile
  listToChan i7zLogLines

i7zCmdArr log = ["sudo", "i7z", "--nogui", "-w", "a", "--logfile", log]

execAndThenDie cmdArr = do
  parentThreadId <- myThreadId
  forkIO $ do
    setCurrentDirectory "/tmp"
    void $ readProc cmdArr
    killThread parentThreadId

tailFile :: String -> IO [String]
tailFile f = systemReadLines $ "tail -n 0 -F " ++ f ++ " 2>/dev/null"

numbers s = concat groupSets
  where groupSets = map tail (regexAllSubmatches p s)
        p = "(\\d+(?:\\.\\d+)?)"

toDouble = read :: String -> Double

--"bignumber Mhz Mhz ... " -> [Mhz]
parseI7zLog :: String -> [Int]
parseI7zLog line = filter (<10^9) $ map (round.toDouble) $ numbers line
