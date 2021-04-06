module PidginPipe(pidginPipeW) where
import Clickable (clickableAsync)
import Label (mainLabel)
import Image (imageW)
import System.Environment (getEnv)
import Control.Monad (forever)
import Data.Char (toLower)
import Utils (getHomeFile, ifM, selectClosestImageDir, chompAll, isRunning, chompFile)

main = mainLabel $ getImage 0
pidginPipeW h = clickableAsync clickL clickM clickR =<< imageW (getImage h)

exec = "pidgin"
process = exec
workspace = 2

runCmd = "daemon " ++ exec
wsCmd = "wmctrl -s " ++ show (workspace-1)
killCmd = "pkill " ++ process ++ " ; sleep 0.1 ; pidgin-matrix-log-cleanup"
rerunCmd = killCmd ++ "; " ++ runCmd

clickL = ifM (isRunning process) (return $ Just wsCmd) (return $ Just runCmd)
clickM = return $ Just rerunCmd
clickR = return $ Just killCmd

getImage :: Int -> IO String
getImage h = do
  pipeFile <- getHomeFile ".purple/plugins/pipe"

  pipe <- chompFile pipeFile
  let status = if null pipe then "off" else map toLower pipe

  pidginRunning <- if status == "off" then return False else isRunning exec

  dir <- selectClosestImageDir h
  let img = if pidginRunning then imgName status else imgName "off"
  return $ dir ++ "/pidgin/" ++ img ++ ".png"

imgName status = case status of
  "off"            -> "not-running"
  "new message"    -> "pidgin-tray-pending"
  "available"      -> "pidgin-tray-available"
  "away"           -> "pidgin-tray-away"
  "do not disturb" -> "pidgin-tray-busy"
  "invisible"      -> "pidgin-tray-invisible"
  "offline"        -> "pidgin-tray-offline"
  _                -> "pidgin-tray-xa"

