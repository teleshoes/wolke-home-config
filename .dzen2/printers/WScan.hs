module WScan(main) where
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode (ExitSuccess))
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import Control.Monad (void)
import Control.Monad.Loops (whileM, iterateUntil)
import System.Posix (sleep)
import Text.Regex.PCRE
import ClickAction (clickAction)

success = (==) ExitSuccess

main = do
  putStrLn $ "Wifi Scan"
  putStrLn $ "<last scan below, fetching new now>"
  old <- readProcess "wscan" ["-l"] ""
  putStrLn $ formatWscan old
  iterateUntil success (do
    hFlush stdout
    (exitCode, out, err) <- readProcessWithExitCode "wscan" [] ""
    if success exitCode
    then putStrLn $ "^cs()\n" ++ formatWscan out
    else (do
      putStrLn $ "err: " ++ errorMsg err
      void $ sleep 1)
    return exitCode)

formatWscan = id
--  title = "Wifi Scan: " ++ concat  (map wrapWscan) . lines

--getSavedSSIDs = catMaybes . (map isSaved)

maybeSavedSSID line = if saved then Just (matches !! 0) else Nothing
  where matches = getMatches line "(.{20}) \\| (.{7}) \\| (.{4}) \\| (.{5})"
        hasAuto autoStr = autoStr =~ "^\\[" :: Bool
        saved = (length matches == 4) && (hasAuto $ matches !! 3)

sub offset len = (take len) . (drop offset)

errorMsg msg = fromMaybe msg $ getMatch msg re
  where re = "[a-zA-Z0-9]+" ++ "\\s*" ++
             "Interface doesn't support scanning : " ++
             "(.*)"

getMatch a re = listToMaybe $ getMatches a re

getMatches a re = concatMap tail groups
  where groups = a =~ re :: [[String]]



