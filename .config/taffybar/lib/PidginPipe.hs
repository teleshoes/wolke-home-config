module PidginPipe(pidginPipeW) where
import Clickable (clickableAsync)
import Label (mainLabel)
import Image (imageWithStateW)
import System.Environment (getEnv)
import Control.Monad (forever)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Utils (getHomeFile, ifM, selectClosestImageDir, chompAll, isRunning, chompFile)

main = do
  flashToggle <- newToggle
  mainLabel $ getImage 0 flashToggle

pidginPipeW h = do
  flashToggle <- newToggle
  img <- imageWithStateW flashToggle (getImage h)
  clickableAsync clickL clickM clickR img

exec = "pidgin"
process = exec
workspace = 2

runCmd = "daemon tsocks-maybe " ++ exec
wsCmd = "wmctrl -s " ++ show (workspace-1)
killCmd = "pkill " ++ process ++ " ; sleep 0.1 ; pidgin-matrix-log-cleanup"
rerunCmd = killCmd ++ "; " ++ runCmd

clickL = ifM (isRunning process) (return $ Just wsCmd) (return $ Just runCmd)
clickM = return $ Just rerunCmd
clickR = return $ Just killCmd

getImage :: Int -> IORef Bool -> IO String
getImage h flashToggle = do
  pipeFile <- getHomeFile ".purple/plugins/pidgin-pipe-status-pipe-status"

  pipe <- chompFile pipeFile
  let status = if null pipe then "off" else map toLower pipe

  isFlash <- nextToggle flashToggle

  pidginRunning <- if status == "off" then return False else isRunning exec

  dir <- selectClosestImageDir h
  let img = if pidginRunning then imgName status isFlash else imgName "off" isFlash
  return $ dir ++ "/pidgin/" ++ img ++ ".png"

newToggle :: IO (IORef Bool)
newToggle = newIORef False

nextToggle :: IORef Bool -> IO Bool
nextToggle toggle = do
  curVal <- readIORef toggle
  let nextVal = not curVal
  writeIORef toggle nextVal
  return nextVal

imgName status isFlash = case status of
  "off"            -> "off"
  "new"            -> if isFlash then "new-other-invert" else "new-other"
  "new-important"  -> if isFlash then "new-important-invert" else "new-important"
  "new-bot"        -> "new-bot" --skip flash
  "available"      -> "pidgin-tray-available"
  "away"           -> "pidgin-tray-away"
  "do not disturb" -> "pidgin-tray-busy"
  "invisible"      -> "pidgin-tray-invisible"
  "offline"        -> "pidgin-tray-offline"
  _                -> "pidgin-tray-available"

