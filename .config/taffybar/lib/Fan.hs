module Fan(fanW) where
import Label (labelW, mainLabel)
import Utils (fg, bg, padL, regexGroups,
  readInt, readDouble, chompFile, readProc)
import Data.Maybe (fromMaybe)
import System.Process (system)

main = mainLabel fanReader
fanW = labelW fanReader

width = 2

fanCmd = ["fan", "--get"]
tempCmd = ["acpi", "-V"]

fanReader = do
  system "sudo fan --reapply"
  fanInfo <- readProc fanCmd
  tempInfo <- readProc tempCmd
  let temp = parseCpuTemp tempInfo
  let (speed, level) = parseFanInfo fanInfo
  return $ formatInfo temp speed level

parseCpuTemp :: String -> Double
parseCpuTemp tempInfo = fromMaybe 0 $ readDouble $ grps!!0
  where re = ", (\\d+\\.\\d+) degrees C"
        grps = fromMaybe [] $ regexGroups re tempInfo

parseFanInfo :: String -> (Integer, String)
parseFanInfo fanInfo = (speed, level)
  where re = "^(\\d+),(\\w+)$"
        grps = fromMaybe ["-1", "??"] $ regexGroups re fanInfo
        speed = fromMaybe (-1) $ readInt $ grps!!0
        level = grps!!1

formatInfo temp speed level = tempFmt ++ "\n" ++ speedFmt
  where tempFmt = colorTemp temp $ padNum width tempText
        speedFmt = colorFan level $ padNum width speedText
        speedText = if speed >= 10000 then "??" else show $ speed`div`100
        tempText = if temp >= 100 then "!!" else show temp
        padNum width text = padL '0' width $ take width text

colorTemp temp | temp > 80 = bg "red" . fg "#002b36"
               | temp > 60 = bg "orange" . fg "#002b36"
               | otherwise = fg "white"

colorFan level = case level of
                   "auto"       -> fg "#268bd2"
                   "disengaged" -> fg "white"
                   "0"          -> bg "red" . fg "#002b36"
                   "1"          -> bg "orange" . fg "#002b36"
                   "2"          -> bg "orange" . fg "#002b36"
                   "3"          -> bg "orange" . fg "#002b36"
                   "4"          -> bg "orange" . fg "#002b36"
                   "5"          -> bg "orange" . fg "#002b36"
                   "6"          -> bg "orange" . fg "#002b36"
                   "7"          -> bg "black" . fg "white"
                   _            -> bg "yellow" . fg "red"
