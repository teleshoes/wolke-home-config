import Prelude hiding ( catch )
import Control.Exception
import System.IO
import System.Process(runCommand, readProcessWithExitCode)
import Data.Maybe (fromMaybe)

import Text.Regex.PCRE

import TextRows(textRows)
import ClickAction (clickAction)

import Data.Time.Calendar (toModifiedJulianDay, fromGregorian, showGregorian)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time
import Locale


main = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  _ <- runCommand "sudo fcron"
  (_, fcrondynOut, _) <- fcrondynExec
  putStr $ clickAction "1" cmd $ parseAndFormat now tz fcrondynOut

fcrondynExec = readProcessWithExitCode "sudo" ["fcrondyn", "-x", "ls"] ""

cmd = ""
      ++ "gnome-terminal -x sh -c \""
      ++ "vim $HOME/.fcrontab; fcronreset; echo OK; read STDIN"
      ++ "\""

parseAndFormat now tz fcrondynOut = rows now tz okJobs
  where jobs = map jobGroups $ lines fcrondynOut
        okJobs = map head $ filter ((==1).length) jobs

rows now tz (one:two:[]) = textRows
                             (pad (fmt one) 10)
                             (pad (fmt two) 10)
                             36
  where fmt job = (relTime (jobTime tz job) now) ++ "|" ++ (jCmd job)
rows now tz (one:[]) = textRows
                         (pad (jCmd one) 10)
                         (pad (relTime (jobTime tz one) now) 10)
                         36
rows now tz _ = "No jobs"

jCmd job = cmdSub $ jobCmd job

cmdSub cmd = if isMatch then (head match !! 1) else cmd
  where match = cmd =~ regex :: [[String]]
        isMatch = length match == 1
        regex = "#([a-zA-Z_0-9]+)"


pad x i | length x >= i = x
pad x i = pad x (i-1) ++ " "

jobCmd  (whole:id:user:mon:day:year:h:m:s:cmd:[]) = cmd
jobTime tz (whole:id:user:mon:day:year:h:m:s:cmd:[]) = t
  where t = utcTime tz h m s mon day year

relTime t1 t2 = (str "d" d) ++ (str "h" h) ++ (str "m" m) ++ (str "s" s)
  where sex = round $ diffUTCTime t1 t2
        s = sex `mod` 60
        m = sex `div` 60 `mod` 60
        h = sex `div` 60 `div` 60 `mod` 60 `mod` 24
        d = sex `div` 60 `div` 60  `div` 24
        str p 0 = ""
        str p x = (show x) ++ p


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

