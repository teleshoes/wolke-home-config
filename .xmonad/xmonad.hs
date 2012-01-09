import Bindings (myKeyBindings, myMouseBindings)
import Dzen (spawnHookedDzens, spawnUnhookedDzens, myDzenLogHook)

import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators ( (|||), JumpToLayout(..))

import XMonad.Hooks.ManageDocks (avoidStruts, SetStruts(..))
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Types (Direction2D(U,D,L,R))

import qualified XMonad.StackSet as Stk

import Control.Concurrent (threadDelay)
import Data.Monoid (All(All))

myHandleEventHook _ = return (All True)

workspaceNames = ["A", "B", "D", "G", "5", "6", "7", "8", "9"]

main = do
  --remove intermediate haskell compilation files
  spawn "find $HOME/.xmonad/ -regex '.*\\.\\(hi\\|o\\)' -delete"

  --clean workspace-images
  safeSpawn "workspace-image" ("init":workspaceNames)

  spawn "killall dzen2"

  spawnUnhookedDzens

  hookedDzens <- spawnHookedDzens

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
                         [ className =? "Eclipse"        --> doShift "A"
                         , className =? "Pidgin"         --> doShift "B"
                         , className =? "MPlayer"        --> doShift "7"
                         , className =? "Thunderbird"    --> doShift "8"
                         , className =? "Rhythmbox"      --> doShift "9"
                         , title     =? "xmonad-hidden"  --> doHide
                         , title     =? "KLOMP"          --> doShift "9"
                         , title     =? "Close Firefox"  --> restartFF
                         , title     =? "npviewer.bin"   --> doFull -- flash
                         ],

    handleEventHook    = myHandleEventHook,
    logHook            = myDzenLogHook workspaceNames hookedDzens
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

addStruts = SetStruts [U,D,L,R] []
removeStruts = SetStruts [] [U,D,L,R]

doHide = ask >>= doF . Stk.delete
doView workspace = doF $ Stk.view workspace
doShiftView workspace = doShift workspace <+> doView workspace
doFull = do
  liftX $ sendMessage $ removeStruts
  (liftX . sendMessage . JumpToLayout) "full"
  doF id
