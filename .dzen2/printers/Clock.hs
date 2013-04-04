module Clock(main) where
import System.Environment (getEnv)
import ClickAction (clickAction)
import TextRows (textRows)
import Utils (chompProc)

main = do
 home <- getEnv "HOME"
 top <- chompProc ["date", "+%a %b %d"]
 bot <- chompProc ["date", "+%H:%M:%S"]
 putStr $ clickAction 1 (cmd home) (textRows top bot)

cmd home = calCmd ++ " | " ++ popupCmd ++ " 1800 48 -fn inconsolata-14"
  where calCmd = home ++ "/.dzen2/printers/ghcprinter Calendar"
        popupCmd = home ++ "/.dzen2/launchers/popup"

