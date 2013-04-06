module PidginPipe(pidginPipeW) where
import Widgets (pollingImageNew, clickable)
import System.Environment (getEnv)
import Data.Char (toLower)
import Utils (height, chompAll, isRunning, chompFile)

clickL = Just $ ""
                ++ " sleep 0.1;"
                ++ " pkill -0 pidgin"
                ++ " && xdotool key --clearmodifiers alt+2"
                ++ " || pidgin"
clickM = Just "killall pidgin; pidgin"
clickR = Just "killall pidgin"

pidginPipeW = do
  img <- pollingImageNew 1 getImage
  click <- clickable img clickL clickM clickR
  return click

getImage = do
  home <- getEnv "HOME"
  let iconSubdir = show height ++ "x" ++ show height
  let dir = home ++ "/.dzen2/icons/" ++ iconSubdir ++ "/pidgin"
  let pipeFile = home ++ "/.purple/plugins/pipe"

  pipe <- chompFile pipeFile
  let status = if null pipe then "off" else map toLower pipe

  pidginRunning <- if status == "off" then return False else isRunning "pidgin"

  let img = if pidginRunning then imgName status else imgName "off"
  return $ dir ++ "/" ++ img ++ ".xpm"

imgName status = case status of
  "off"            -> "not-running"
  "new message"    -> "pidgin-tray-pending"
  "available"      -> "pidgin-tray-available"
  "away"           -> "pidgin-tray-away"
  "do not disturb" -> "pidgin-tray-busy"
  "invisible"      -> "pidgin-tray-invisible"
  "offline"        -> "pidgin-tray-offline"
  _                -> "pidgin-tray-xa"

