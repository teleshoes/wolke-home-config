module Volume (main, getVol, isMuted) where
import System.Environment (getEnv)
import System.Process(readProcess)
import PercentBar (percentBar)
import Text.Regex.PCRE ((=~))

mutedColors = ["yellow", "red", "pink"] ++ repeat "orange"
unmutedColors = ["black", "green", "blue"] ++ repeat "orange"

main = do
  home <- getEnv "HOME"
  vol <- getVol "speaker"
  mute <- isMuted "speaker"
  let colors = if mute then mutedColors else unmutedColors
  putStr $ percentBar vol colors 5 3

getVol :: String -> IO Int
getVol = fmap fst . getStatus

isMuted :: String -> IO Bool
isMuted = fmap snd . getStatus

getStatus :: String -> IO (Int, Bool)
getStatus dev = do
  status <- readProcess "pulse-vol" [dev] ""
  let parsed = status =~ "(\\d+) \\((muted|unmuted|unknown)\\)" :: [[String]]
  let vol = parsed !! 0 !! 1
  let mute = parsed !! 0 !! 2
  return (read vol, mute == "muted")


