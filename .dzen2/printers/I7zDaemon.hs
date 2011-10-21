import System.Process (system)
import System.Posix.Process (getProcessID, forkProcess, executeFile)
import System.Unix.Process (killByCwd)
import System.Directory (setCurrentDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when, void)


basedir = "/tmp/i7z"
pidfile = basedir ++ "/pid"
logFile subdir = subdir ++ "/cpufreq.txt"

i7zCmdArr dir =
  [ "gnome-terminal"
  , "--working-directory", dir
  , "--title", "xmonad-hidden"
  , "-e", "sudo i7z -w l > /dev/null"
  ]
mkdirExec dir = "mkdir -p " ++ dir ++ " > /dev/null"

forkExec (cmd:args) = forkProcess $ executeFile cmd True args Nothing

toInt = read :: String -> Int
isInt = maybe False id . fmap (null . snd) . listToMaybe . (reads :: ReadS Int)

sys = (>> return ()) . system

maybeWriteFile :: FilePath -> String -> IO ()
maybeWriteFile f s = catch (writeFile f s) (\_ -> return ())

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile f = catch (Just <$> readFile f) (\_ -> return Nothing)

parsePidFile :: Maybe String -> Int
parsePidFile Nothing = -1
parsePidFile (Just out) = pid
  where outLines = filter (not.null) $ lines $ out
        onlyLine = if length outLines == 1 then head outLines else ""
        pid = if isInt onlyLine then toInt onlyLine else -1

main = do
  pid <- getProcessID
  
  pidfileOut <- maybeReadFile pidfile
  let oldpid = parsePidFile pidfileOut
  when (oldpid > 0) $ void (putStrLn $ basedir ++ "/" ++ show oldpid)
  when (oldpid > 0) $ void (killByCwd $ basedir ++ "/" ++ show oldpid)

  let dir = basedir ++ "/" ++ show pid
  system $ mkdirExec dir
  
  maybeWriteFile pidfile $ show pid
  
  i7zPid <- forkExec $ i7zCmdArr dir
  print pid


--  forever $ putStrLn $ i7zExec

