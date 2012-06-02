module CpuScalingGov(main) where
import Utils (fg, bg, padL, readInt, chompFile, readProc)
import TextRows (textRows)

import Control.Monad (void)
import System.Process (system)
import System.Environment (getEnv)
import Data.Maybe (fromMaybe, isJust, listToMaybe)

width = 2

cpuDir = "/sys/devices/system/cpu"
okGovs = ["powersave", "performance", "ondemand", "conservative", "userspace"]

setGov g = system $ "sudo cpu-set " ++ g

curGov = do
  let regex = cpuDir ++ "/cpu[0-9]+/cpufreq/scaling_governor"
  cpuGovDevs <- fmap lines $ readProc ["find", cpuDir, "-regex", regex]
  cpuGovs <- mapM chompFile cpuGovDevs
  let cpuGov = fromMaybe "?" $ listToMaybe cpuGovs
  return $ if any ((/=) cpuGov) cpuGovs then "?" else cpuGov

main = do
  home <- getEnv "HOME"
  cur <- curGov
  last <- chompFile $ home ++ "/.cpu_governor"
  let (curOk, lastOk) = (cur `elem` okGovs, last `elem` okGovs)

  let govDisp = if curOk && lastOk && cur == last then cur else "!"

  if lastOk && cur /= last then void $ setGov last else return ()
  if not lastOk && curOk then void $ setGov cur else return ()

  let spdFile = cpuDir ++ "/cpu0/cpufreq/scaling_setspeed"
  spd <- if cur == "userspace" then chompFile spdFile else return ""
  putStrLn $ formatGovernor govDisp (readInt spd)

formatGovernor g spd = fg "black" $ bg color $ textRows (pad top) (pad bot)
  where spdKHhz = (fromMaybe 0 spd) `div` 10000
        isTurboBoost = ((fromMaybe 0 spd) `mod` 10000) > 0
        turboDisp = if isTurboBoost then "turbo" else ""
        color = govColor g
        pad = padL ' ' width
        (top, bot) = if g == "userspace" then
                       (take width $ show spdKHhz, take width turboDisp)
                     else
                       (take width g, take width $ drop width g)

govColor gov = case gov of
  "powersave"    -> "red"
  "performance"  -> "green"
  "ondemand"     -> "blue"
  "conservative" -> "yellow"
  "userspace"    -> "purple"
  _              -> "white"
