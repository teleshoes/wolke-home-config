module PidginPipe(main) where
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Data.Char (toLower)
import ClickableImage (clickableImage)
import Utils (height, chompAll, isRunning)

clickCommands = [ ""
                  ++ "pidof pidgin; "
                  ++ "if [ $? == 0 ]; then "
                    ++ "xdotool key --clearmodifiers alt+2; "
                  ++ "else "
                    ++ "pidgin; "
                  ++ "fi"
                , "killall pidgin; pidgin"
                , "killall pidgin"
                ]

main = do
  home <- getEnv "HOME"
  let iconSubdir = show height ++ "x" ++ show height
  let dir = home ++ "/.dzen2/icons/" ++ iconSubdir ++ "/pidgin"
  let pipeFile = home ++ "/.purple/plugins/pipe"

  pipeExists <- doesFileExist pipeFile
  pipe <- if pipeExists then readFile pipeFile else return "off"
  let status = map toLower $ chompAll pipe

  pidginRunning <- if status == "off" then return False else isRunning "pidgin"

  let img = if pidginRunning then imgName status else imgName "off"
  putStrLn $ clickableImage clickCommands (dir ++ "/" ++ img ++ ".xpm")

imgName status = case status of
  "off"            -> "not-running"
  "new message"    -> "pidgin-tray-pending"
  "available"      -> "pidgin-tray-available"
  "away"           -> "pidgin-tray-away"
  "do not disturb" -> "pidgin-tray-busy"
  "invisible"      -> "pidgin-tray-invisible"
  "offline"        -> "pidgin-tray-offline"
  _                -> "pidgin-tray-xa"

