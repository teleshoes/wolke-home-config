module Clock(main) where
import System.Environment (getEnv)
import System.Process(readProcess)
import ClickAction (clickAction)
import TextRows (textRows)

main = do
 home <- getEnv "HOME"
 top <- readProcess "date" ["+%a %b %d"] ""
 bot <- readProcess "date" ["+%H:%M:%S"] ""
 putStr $ clickAction 1 (cmd home) (textRows top bot)

cmd home = calCmd ++ " | " ++ popupCmd ++ " 1800 48 -fn inconsolata-14"
  where calCmd = home ++ "/.dzen2/printers/ghcprinter Calendar"
        popupCmd = home ++ "/.dzen2/launchers/popup"

