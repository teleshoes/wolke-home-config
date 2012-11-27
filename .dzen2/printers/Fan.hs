module Fan(main) where
import Utils (fg, bg, padL, regexGroups, readInt, chompFile)
import TextRows (textRows)
import Data.Maybe (fromMaybe)

width = 2

fanDev = "/proc/acpi/ibm/fan"

main = do
  info <- chompFile fanDev
  let (status, speed, level) = parseFanInfo info
  putStrLn $ formatScaling status speed level

parseFanInfo info = (grps!!0, fromMaybe 0 $ readInt $ grps!!1, grps!!2)
  where re = ""
             ++ "status:\\s*(.*)\\n?"
             ++ "speed:\\s*(\\d+)\\n?"
             ++ "level:\\s*(.*)\\n?"
        grps = fromMaybe [] $ regexGroups re info

formatScaling status speed level = col $ textRows (pad top) (pad bot)
  where col = color level
        pad = padL '0' width . take width
        (top, bot) = (take 2 level, show $ speed `div` 100)

color level = case level of
                "auto"       -> bg "blue"
                "disengaged" -> fg "white"
                "0"          -> bg "red" . fg "black"
                "7"          -> bg "black" . fg "white"
                _            -> bg "orange" . fg "black"
