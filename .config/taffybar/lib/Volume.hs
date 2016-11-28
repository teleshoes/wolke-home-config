module Volume (volumeW, getVol, isMuted) where
import PercentBarWidget (percentBarWidgetW, mainPercentBarWidget)
import Color as C
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)
import Utils (regexGroups, readProc)

main = mainPercentBarWidget 0.5 $ readVolBar "speaker"
volumeW = percentBarWidgetW 0.5 $ readVolBar "speaker"

mutedColors = map C.rgb [C.Yellow, C.Red] ++ otherColors
unmutedColors = map C.rgb [C.Black, C.Green] ++ otherColors
otherColors = map C.rgb $ take 10 $ cycle [C.Blue, C.Orange]

readVolBar dev = do
  (vol, mute) <- getStatus dev
  let p = (fromIntegral vol)/100.0
  let colors = if mute then mutedColors else unmutedColors
  return (p, colors)

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
