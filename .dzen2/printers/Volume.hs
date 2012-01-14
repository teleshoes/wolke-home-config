module Volume (main) where
import System.Environment (getEnv)
import System.Process(readProcess)
import PercentBar (percentBar)

mutedColors = ["yellow", "red", "pink"] ++ repeat "orange"
unmutedColors = ["black", "green", "blue"] ++ repeat "orange"

main = do
  home <- getEnv "HOME"
  vol <- fmap i $ readProcess (home ++ "/bin/pulse-volume") [] ""
  mute <- fmap isMuted $ readProcess (home ++ "/bin/pulse-mute") ["sink"] ""
  let colors = if mute then mutedColors else unmutedColors
  putStr $ percentBar vol colors 5 3

i = read :: String -> Int

isMuted sink | (lines sink !! 0) == "sink is muted" = True
             | (lines sink !! 0) == "sink is not muted" = False
             | otherwise = error ("unknown speaker status: " ++ sink)

