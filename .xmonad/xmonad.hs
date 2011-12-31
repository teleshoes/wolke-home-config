import Bindings
import Dzen

import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators ( (|||) )

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Run (safeSpawn)

import qualified XMonad.StackSet as Stk

import Control.Concurrent (threadDelay)
import Data.Monoid
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import DBus.Client.Simple
import System.Taffybar.XMonadLog (dbusLog)

myHandleEventHook _ = return (All True)

workspaceNames = ["A", "B", "D", "G", "5", "6", "7", "8", "9"]

closeRboxWin = "xdotool search --class Rhythmbox key --window %@ ctrl+w"


main = do
  safeSpawn "xsetroot" ["-cursor_name", "left_ptr"]
  --clean workspace-images
  _ <- safeSpawn "workspace-image" ("init":workspaceNames)
  --clean
  _ <- spawn $ "cd $HOME/.xmonad; " ++
                   "find -regex '.*\\.\\(hi\\|o\\)' " ++
                   "-execdir rm {} \\;"
  --kill running dzens
  _ <- spawn "killall dzen2"
  --start the unhooked dzens
  _ <- spawnUnhookedDzens
  hookedDzens <- spawnHookedDzens

  dbusClient <- connectSession
  
  xmonad $ defaultConfig {
    focusFollowsMouse  = False,
    modMask            = mod1Mask,
    workspaces         = workspaceNames,
    
    borderWidth        = 3,
    normalBorderColor  = "#dddddd",
    focusedBorderColor = "#ff0000",

    keys               = myKeyBindings,
    mouseBindings      = myMouseBindings,
  
    layoutHook         = avoidStruts $ smartBorders $
                             (named "left" $          Tall 1 (3/100) (55/100))
                         ||| (named "top"  $ Mirror $ Tall 1 (3/100) (55/100))
                         ||| (named "full" $          Full)
                         ,

    manageHook         = composeAll
                         [ className =? "Gnome-panel"    --> doIgnore
                         , className =? "Do"             --> doIgnore
                         , className =? "Eclipse"        --> doShift "A"
                         , className =? "Pidgin"         --> doShift "B"
                         , className =? "MPlayer"        --> doShift "7"
                         , className =? "Thunderbird"    --> doShift "8"
                         , className =? "Rhythmbox"      --> doShift "9"
                         , title     =? "xmonad-hidden"  --> doHide
                         , title     =? "KLOMP"          --> doShift "9"
                         , title     =? "Close Firefox"  --> restartFF
                         ],

    handleEventHook    = myHandleEventHook,
    logHook            = do myDzenLogHook workspaceNames hookedDzens
                            dbusLog dbusClient defaultPP
  }

restartFF = do 
  w <- ask
  let delay = 1
  liftX $ do
    killWindow w
    io . threadDelay $ delay*10^6
    spawn $ "notify-send 'restarting firefox in " ++ show delay ++ "s'"
    spawn "firefox"
    refresh
  doF id

doHide = ask >>= doF . Stk.delete
doView workspace = doF $ Stk.view workspace
doShiftView workspace = doShift workspace <+> doView workspace

