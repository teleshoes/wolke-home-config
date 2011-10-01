import System.Environment (getEnv)
import System.Process(readProcess)
import CommandClick (imgDir)
import ClickAction (clickAction)
import ClickableImage (clickableImage)

main = do
 home <- getEnv "HOME"
 mic <- readProcess (home ++ "/bin/pulse-mute") ["microphone"] ""
 putStr $ format' home (isMuted $ lines mic !! 0)

format home isM = clickableImage [clickCmd] $ imgDir home ++ img
  where img | isM     = "microphone-muted.xpm"
            | not isM = "microphone-unmuted.xpm"

format' home isM = clickAction "1" clickCmd markup
  where markup = "^bg(black)^fg(" ++ color ++ ")M^fg()^bg()"
        color = if isM then "red" else "green"

isMuted mic | mic == "source is muted" = True
            | mic == "source is not muted" = False
            | otherwise = error ("unknown microphone status: " ++ mic)

clickCmd = "pulse-mute microphone toggle"
