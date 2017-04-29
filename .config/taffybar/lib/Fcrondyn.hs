module Fcrondyn(fcrondynW) where
import Clickable (clickableLeft)
import Label (labelW, mainLabel)
import System.IO
import System.Process(readProcessWithExitCode, system)
import Data.Maybe (catMaybes, fromMaybe, fromJust, isJust)
import Control.Monad (void)

import Utils(isRunning, padR, readProc)

import Data.Time.Calendar (toModifiedJulianDay, fromGregorian, showGregorian)
import Data.Time
import Utils (regexGroups, regexFirstGroup)

main = mainLabel fcrondynReader
fcrondynW = clickableLeft cmd =<< labelW fcrondynReader

fcrondynReader = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  system "sudo fcron-start >/dev/null"
  fcrondynOut <- readProc ["fcron-ls"]
  return $ parseAndFormat now tz fcrondynOut

cmd = "term \"fcron-tool --edit; echo; echo ENDED; read STDIN\""

parseAndFormat now tz fcrondynOut = rows now tz (namedJobs okJobs)
  where okJobs = catMaybes $ map (regexGroups regex) $ lines fcrondynOut
        regex = ""
                ++ "(\\d+)\\s*\\|"
                ++ "(\\w+)\\s*\\|"
                ++ "(\\d+)-(\\d+)-(\\d+)\\s*"
                ++ "(\\d+):(\\d+):(\\d+)\\s*\\|"
                ++ "(.*)"


namedJobs [] = []
namedJobs (j:js) | isJust mName = (j, fromJust mName):(namedJobs js)
                 | otherwise = namedJobs js
  where mName = maybeJobName j

rows now tz jobs = (padR ' ' 14 top) ++ "\n" ++ (padR ' ' 14 bot)
  where fmt (j,name) = (time j) ++ "|" ++ name
        time j = padR ' ' 11 $ relTime (jobTime tz j) now
        top = if length jobs > 0 then fmt $ jobs !! 0 else "No jobs"
        bot = if length jobs > 1 then fmt $ jobs !! 1 else ""

maybeJobName job = regexFirstGroup "#([a-zA-Z0-9]{2})$" $ jobCmd job

jobCmd  (id:user:year:mon:day:h:m:s:cmd:[]) = cmd
jobTime tz (id:user:year:mon:day:h:m:s:cmd:[]) = t
  where t = utcTime tz h m s year mon day

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


utcTime tz h m s year mon day = localTimeToUTC tz $ LocalTime jDay tod
  where jDay = fromGregorian (i year) (int mon) (int day)
        tod = TimeOfDay (int h) (int m) (f s)

f = realToFrac . read
i = read :: String -> Integer
int = read :: String -> Int

hdr = "ID   |USER     |SCHEDULE        |CMD"

