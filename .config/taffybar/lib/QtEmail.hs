module QtEmail(qtemailW) where
import Color (Color(..), hexColor, widgetBgColorWrap)
import Clickable (clickable)
import Image (imageW)
import Label (labelW, mainLabel)
import Graphics.UI.Gtk (containerAdd, hBoxNew)
import Utils (
  imageDir, fg, isRunning, chompFile)

import System.Environment (getEnv)

main = mainLabel $ statusShortMarkup $ hexColor White
qtemailW h fgColor bgColor = do
  img <- imageW (getImage h)
  label <- labelW $ statusShortMarkup $ hexColor fgColor

  box <- hBoxNew False 0
  containerAdd box img
  containerAdd box label

  widgetBgColorWrap bgColor =<< clickable clickL clickM clickR box

exec = "email-gui.py"
process = exec

clickL = Just $ exec
clickM = Nothing
clickR = Just $ "pkill " ++ process

getImage h = do
  running <- isRunning process
  dir <- imageDir h
  let img = if running then "qtemail-on.png" else "qtemail-off.png"
  return $ dir ++ "/" ++ img

statusShortMarkup color = do
  home <- getEnv "HOME"
  statusShort <- chompFile $ home ++ "/.cache/email/status-short"
  return $ fg color statusShort
