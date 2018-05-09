module QtEmail(qtemailW) where
import Color (Color(..), hexColor, widgetBgColorWrap)
import Clickable (clickableAsync)
import Image (imageW)
import Label (labelW, mainLabel)
import Graphics.UI.Gtk (containerAdd, hBoxNew)
import Utils (
  ifM, imageDir, fg, isRunning, chompFile)

import System.Environment (getEnv)

main = mainLabel $ statusShortMarkup $ hexColor White
qtemailW h fgColor bgColor = do
  img <- imageW (getImage h)
  label <- labelW $ statusShortMarkup $ hexColor fgColor

  box <- hBoxNew False 0
  containerAdd box img
  containerAdd box label

  widgetBgColorWrap bgColor =<< clickableAsync clickL clickM clickR box

exec = "email-gui.py"
process = exec
workspace = 8

runCmd = "daemon " ++ exec
wsCmd = "wmctrl -s " ++ show (workspace-1)

clickL = ifM (isRunning process) (return $ Just wsCmd) (return $ Just runCmd)
clickM = return Nothing
clickR = return $ Just $ "pkill " ++ process

getImage h = do
  running <- isRunning process
  dir <- imageDir h
  let img = if running then "qtemail-on.png" else "qtemail-off.png"
  return $ dir ++ "/" ++ img

statusShortMarkup color = do
  home <- getEnv "HOME"
  statusShort <- chompFile $ home ++ "/.cache/email/status-short"
  return $ fg color statusShort
