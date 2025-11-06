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

fanReader = do
  system "sudo fan --reapply"
  fanInfo <- readProc fanCmd
  let (level, speed, temp) = parseFanInfo fanInfo
  return $ formatInfo temp speed level

parseFanInfo :: String -> (String, Integer, Integer)
parseFanInfo fanInfo = (level, speed, temp)
  where re = "^(\\w+)\\s*:\\s*(\\d+)RPM[^:]*:\\s*(\\d+|\\d+)C\\s*$"
        grps = fromMaybe ["??", "-1", "-1"] $ regexGroups re fanInfo
        level = grps!!0
        speed = fromMaybe (-1) $ readInt $ grps!!1
        temp = fromMaybe (-1) $ readInt $ grps!!2

formatInfo temp speed level = tempFmt ++ "\n" ++ speedFmt
  where tempFmt = colorTemp temp $ padNum width tempText
        speedFmt = colorFan level $ padNum width speedText
        speedText = if speed >= 10000 then "??" else show $ speed`div`100
        tempText = if temp >= 100 then "!!" else show temp
        padNum width text = padL '0' width $ take width text

colorTemp temp | temp > 80 = bg "red" . fg "#002b36"
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
