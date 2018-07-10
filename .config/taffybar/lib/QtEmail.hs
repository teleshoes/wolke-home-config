module QtEmail(qtemailW) where
import Clickable (clickableAsync)
import Image (imageW)
import Label (labelW, mainLabel)
import Utils (
  eboxStyleWrapW, ifM, imageDir, fg, getHomeFile, isRunning, chompFile)

import GI.Gtk.Enums (
  Orientation(OrientationHorizontal))
import GI.Gtk.Objects.Box (boxNew, boxSetHomogeneous)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Widget (Widget, toWidget)
import System.Taffybar.Widget.Util (widgetSetClassGI)

import Data.Text (pack)
import System.Environment (getEnv)

main = mainLabel statusShortMarkup

qtemailW h = do
  img <- imageW (getImage h)
  label <- labelW statusShortMarkup

  box <- boxNew OrientationHorizontal 0
  boxSetHomogeneous box False

  containerAdd box img
  containerAdd box label

  box <- clickableAsync clickL clickM clickR box
  eboxStyleWrapW box "Email"

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

statusShortMarkup = do
  statusShortFile <- getHomeFile ".cache/email/status-short"
  statusShort <- chompFile statusShortFile
  return statusShort
