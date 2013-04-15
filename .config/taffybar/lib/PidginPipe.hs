module PidginPipe(pidginPipeW) where
import Widgets (pollingImageNew, clickable)
import System.Environment (getEnv)
import Data.Char (toLower)
import Utils (barImage, chompAll, isRunning, chompFile)

clickL = Just "pkill -0 pidgin && wmctrl -s 1 || pidgin"
clickM = Nothing
clickR = Just "pkill pidgin"

pidginPipeW h = clickable clickL clickM clickR =<< pollingImageNew (getImage h)

getImage h = do
  home <- getEnv "HOME"
  let pipeFile = home ++ "/.purple/plugins/pipe"

  pipe <- chompFile pipeFile
  let status = if null pipe then "off" else map toLower pipe

  pidginRunning <- if status == "off" then return False else isRunning "pidgin"

  let img = if pidginRunning then imgName status else imgName "off"
  barImage h $ "pidgin/" ++ img

imgName status = case status of
  "off"            -> "not-running"
  "new message"    -> "pidgin-tray-pending"
  "available"      -> "pidgin-tray-available"
  "away"           -> "pidgin-tray-away"
  "do not disturb" -> "pidgin-tray-busy"
  "invisible"      -> "pidgin-tray-invisible"
  "offline"        -> "pidgin-tray-offline"
  _                -> "pidgin-tray-xa"

