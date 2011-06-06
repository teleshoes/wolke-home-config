import Bindings
import Dzen

import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators ( (|||) )

import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)

import qualified XMonad.StackSet as Stk

workspaceNames = ["A", "B", "D", "G", "5", "6", "7", "8", "9"]

main = do
  dzenKill <- spawn "killall dzen2"
  hookedDzens <- spawnHookedDzens
  unhookedDzens <- spawnUnhookedDzens
  
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
                         , className =? "Eclipse"        --> doShift "A"
                         , className =? "Rhythmbox"      --> doShift "9"
                         , className =? "Do"             --> doIgnore
                         , className =? "Thunderbird"    --> doShiftView "8"
                         , className =? "MPlayer"        --> doShift "7"
                         ],

    logHook            = myDzenLogHook workspaceNames hookedDzens
  }

doView workspace = doF (Stk.view workspace)
doShiftView workspace = doShift workspace <+> doView workspace

