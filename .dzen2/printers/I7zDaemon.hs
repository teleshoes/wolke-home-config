import System.Process (system, readProcessWithExitCode)
import System.IO (stdin, stdout, stderr)
import System.Exit(ExitCode(ExitSuccess))
import System.Posix (sleep)
import System.Posix.Process (getProcessID, forkProcess, executeFile)
import System.Unix.Process (killByCwd)
import System.Directory (setCurrentDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when, void)
import GHC.IO.Handle (hClose)

basedir = "/tmp/i7z"
logFileName = "cpu_freq_log.txt"

i7zCmdArr dir =
  [ "sh"
  , "-c"
  , "cd " ++ dir ++ "; sudo i7z -w l > /dev/null"
  ]

mkdirExec dir = "mkdir -p " ++ dir ++ " >/dev/null 2>/dev/null"

forkExec (cmd:args) = forkProcess $ do
  hClose stdin
  hClose stdout
  hClose stderr
  readProcessWithExitCode cmd args ""
--  executeFile cmd True args Nothing

toDouble = read :: String -> Double
isDouble = maybe False id . fmap (null . snd) . listToMaybe . (reads :: ReadS Double)

maybeCatFile :: FilePath -> IO (Maybe String)
maybeCatFile f = do
  (exit, out, err) <- readProcessWithExitCode "cat" [f] ""
  return $ if exit == ExitSuccess then Just out else Nothing

main = do
  pid <- getProcessID
  let dir = basedir ++ "/" ++ show pid
  let log = dir ++ "/" ++ logFileName

  system $ mkdirExec dir
  forkExec $ i7zCmdArr dir

  untilReady log

  system $ "stty sane"

  forever $ do
    system "sleep 1"
    freqs <- maybeParseFreqs <$> maybeCatFile log
    if null freqs then putStrLn "blargh" else putStrLn $ show freqs


untilReady log = do
  sleep 1
  putStrLn "waiting"
  mFreqs <- maybeParseFreqs <$> maybeCatFile log
  if null mFreqs then untilReady log else return ()

maybeParseFreqs Nothing = []
maybeParseFreqs (Just out) = if not ok then [] else map show freqs
  where lns = lines out
        ok = all isDouble lns
        freqs = filter (<100000) $ map (round.toDouble) lns
