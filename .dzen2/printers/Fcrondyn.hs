module Fcrondyn(main) where
import System.IO
import System.Process(runCommand, readProcessWithExitCode)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad (void)

import Text.Regex.PCRE

import Utils(isRunning, padR, readProc)
import TextRows(textRows)
import ClickAction (clickAction)

import Data.Time.Calendar (toModifiedJulianDay, fromGregorian, showGregorian)
import Data.Time


main = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  running <- isRunning "fcron"
  if not running then void $ runCommand "sudo fcron" else return ()
  fcrondynOut <- readProc ["sudo", "fcrondyn", "-x", "ls"]
  putStr $ clickAction 1 cmd $ parseAndFormat now tz fcrondynOut

cmd = ""
      ++ "term -e sh -c \""
      ++ "vim $HOME/.fcrontab; fcronreset; echo OK; read STDIN"
      ++ "\""

parseAndFormat now tz fcrondynOut = rows now tz (namedJobs okJobs)
  where jobs = map jobGroups $ lines fcrondynOut
        okJobs = map head $ filter ((==1).length) jobs

namedJobs [] = []
namedJobs (j:js) | isJust mName = (j, fromJust mName):(namedJobs js)
                 | otherwise = namedJobs js
  where mName = maybeJobName j

rows now tz jobs = textRows (padR ' ' 14 top) (padR ' ' 14 bot)
  where fmt (j,name) = (time j) ++ "|" ++ name
        time j = padR ' ' 11 $ relTime (jobTime tz j) now
        top = if length jobs > 0 then fmt $ jobs !! 0 else "No jobs"
        bot = if length jobs > 1 then fmt $ jobs !! 1 else ""

maybeJobName job = cmdSub $ jobCmd job

cmdSub cmd = if isMatch then Just (head match !! 1) else Nothing
  where match = cmd =~ regex :: [[String]]
        isMatch = length match == 1
        regex = "#([a-zA-Z0-9]{2})$"

jobCmd  (whole:id:user:mon:day:year:h:m:s:cmd:[]) = cmd
jobTime tz (whole:id:user:mon:day:year:h:m:s:cmd:[]) = t
  where t = utcTime tz h m s mon day year

showDHMS ("0","00","00","00") = "now"
showDHMS ("0","00","00",s) = (unzero s)++"s"
showDHMS ("0","00",m,s) = (unzero m)++"m" ++ s++"s"
showDHMS ("0",h,m,s) = (unzero h)++"h" ++ m++"m" ++ s++"s"
showDHMS (d,h,m,s) = d++"d" ++ h++"h" ++ m++"m" ++ s++"s"
unzero ('0':ds) = ds
unzero ds = ds

relTime t1 t2 = showDHMS (show d,sh h,sh m,sh s)
  where sex = round $ diffUTCTime t1 t2
        s = sex `mod` 60
        m = sex `div` 60 `mod` 60
        h = sex `div` 60 `div` 60 `mod` 60 `mod` 24
        d = sex `div` 60 `div` 60  `div` 24
        sh s = (if s < 10 then "0" else "") ++ show s


utcTime tz h m s mon day year = localTimeToUTC tz $ LocalTime jDay tod
  where jDay = fromGregorian (i year) (int mon) (int day)
        tod = TimeOfDay (int h) (int m) (f s)

f = realToFrac . read
i = read :: String -> Integer
int = read :: String -> Int

jobGroups line = line =~ regex :: [[String]]
  where regex = ""
                ++ "(\\d+)\\s*"
                ++ "(\\w+)\\s*"
                ++ "(\\d+)/(\\d+)/(\\d+)\\s*"
                ++ "(\\d+):(\\d+):(\\d+)\\s*"
                ++ "(.*)"

hdr = "ID    USER   SCHEDULE         CMD"

