import System.Environment (getEnv)
import System.Process(readProcess)
import ClickAction (clickAction)
import TextRows (textRows)

height = 36

main = do
 home <- getEnv "HOME"
 top <- readProcess "date" ["+%a %b %d"] ""
 bot <- readProcess "date" ["+%H:%M:%S"] ""
 putStr $ clickAction "1" (cmd home) (textRows top bot height)

cmd home = calExec ++ " | " ++ popupExec ++ " 240 48 -fn inconsolata-14"
  where calExec = home ++ "/.dzen2/printers/calendar"
        popupExec = home ++ "/.dzen2/launchers/popup"

