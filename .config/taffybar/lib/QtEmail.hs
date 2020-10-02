module QtEmail(qtemailW) where
import Clickable (clickableAsync)
import Image (imageW)
import Label (labelW, mainLabel)
import Utils (
  eboxStyleWrapW, ifM, selectClosestImageDir, fg, getHomeFile, isRunning, chompFile)

import GI.Gtk.Enums (
  Orientation(OrientationHorizontal))
import GI.Gtk.Objects.Box (boxNew, boxSetHomogeneous)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Widget (Widget, toWidget)
import System.Taffybar.Widget.Util (widgetSetClassGI)

import Data.Text (pack)
import System.Environment (getEnv)

emailGuiWrapperExec = "qtemail-gui-wrapper"
emailGuiExec = "email-gui.py"
workspace = 8

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

binRe = "/(\\S+/)?bin/"
daemonProcRe = "(" ++ binRe ++ ")?" ++ "daemon"
pythonProcRe = "(" ++ binRe ++ ")?" ++ "python[0-9\\.]*"
emailGuiProcRe = "(" ++ binRe ++ ")?" ++ emailGuiExec
emailGuiWrapperProcRe = "(" ++ binRe ++ ")?" ++ emailGuiWrapperExec

runCmd = "daemon " ++ emailGuiWrapperExec
wsCmd = "wmctrl -s " ++ show (workspace-1)

clickL = ifM (isRunning emailGuiExec) (return $ Just wsCmd) (return $ Just runCmd)
clickM = return Nothing
clickR = return $ Just $ ""
  ++ "echo killing " ++ emailGuiExec ++ " and " ++ emailGuiWrapperExec
  ++ " ; pkill -9 -f '^" ++ daemonProcRe ++ " " ++ emailGuiProcRe ++ "( .*)?$'"
  ++ " ; pkill -9 -f '^" ++ pythonProcRe ++ " " ++ emailGuiProcRe ++ "( .*)?$'"
  ++ " ; pkill -9 -f '^"                        ++ emailGuiProcRe ++ "( .*)?$'"
  ++ " ; pkill -9 -f '^" ++ daemonProcRe ++ " " ++ emailGuiWrapperProcRe ++ "( .*)?$'"
  ++ " ; pkill -9 -f '^" ++ pythonProcRe ++ " " ++ emailGuiWrapperProcRe ++ "( .*)?$'"
  ++ " ; pkill -9 -f '^"                        ++ emailGuiWrapperProcRe ++ "( .*)?$'"

getImage h = do
  running <- isRunning emailGuiExec
  dir <- selectClosestImageDir h
  let img = if running then "qtemail-on.png" else "qtemail-off.png"
  return $ dir ++ "/" ++ img

statusShortMarkup = do
  statusShortFile <- getHomeFile ".cache/email/status-short"
  statusShort <- chompFile statusShortFile
  return statusShort
