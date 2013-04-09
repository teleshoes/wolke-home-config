module Volume (volumeW, getVol, isMuted) where
import PercentBarWidget (
  percentBarWidgetW, percentBarConfig, colorMap, cycleColors)
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)
import System.Process(readProcess)
import Utils (regexGroups, readProc)

mutedColors = map colorMap ["yellow", "red"] ++ otherColors
unmutedColors = map colorMap ["black", "green"] ++ otherColors
otherColors = map colorMap $ "blue":(repeat "orange")

volumeW = percentBarWidgetW percentBarConfig 0.5 $ readVolBar "speaker"

readVolBar dev = do
  (vol, mute) <- getStatus dev
  let p = (fromIntegral vol)/100.0
  let (bg, fg) = cycleColors (if mute then mutedColors else unmutedColors) p
  return (fg, bg, p)

getVol :: String -> IO Double
getVol = fmap (/100.0) . fmap fromIntegral . fmap fst . getStatus

isMuted :: String -> IO Bool
isMuted = fmap snd . getStatus

getStatus :: String -> IO (Int, Bool)
getStatus dev = do
  status <- readProc ["pulse-vol", dev]
  let groups = regexGroups "(\\d+) \\((muted|unmuted|unknown)\\)" status
  let [vol, mute] = fromMaybe ["0", "unknown"] groups
  return (read vol, mute == "muted")
