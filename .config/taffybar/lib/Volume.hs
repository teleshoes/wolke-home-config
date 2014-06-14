module Volume (volumeW, getVol, isMuted) where
import PercentBarWidget (
  percentBarWidgetW, percentBarConfig, cycleColors)
import Color as C
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)
import Utils (regexGroups, readProc)

mutedColors = map C.rgb [C.Yellow, C.Red] ++ otherColors
unmutedColors = map C.rgb [C.Black, C.Green] ++ otherColors
otherColors = map C.rgb $ C.Blue:(repeat C.Orange)

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
  let groups = regexGroups
                 "^(\\d+(?:\\.\\d+)?) \\((muted|unmuted|unknown)\\)$"
                 status
  let [vol, mute] = fromMaybe ["0", "unknown"] groups
  return (floor $ (read vol :: Double), mute == "muted")
