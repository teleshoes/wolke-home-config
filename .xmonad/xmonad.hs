import Bindings
import Dzen
import DzenXinerama

import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators ( (|||) )
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Named
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Actions.DeManage (demanage)
import XMonad.Util.Font (encodeOutput)
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run

import qualified XMonad.StackSet as Stk
import Data.Monoid
import Data.Function (on)
import Data.Ord (comparing)
import Data.Map (fromList)
import Data.Maybe (isJust, catMaybes)
import Data.List (sortBy, intercalate)

import Control.Monad (zipWithM_)

import System.Exit

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
                         (named "left" $          Tall 1 (3/100) (55/100)) |||
                         (named "top"  $ Mirror $ Tall 1 (3/100) (55/100)) |||
                         (named "full" $          Full),

    manageHook         = composeAll
                         [ className =? "Gnome-panel"    --> doIgnore
                         , className =? "Eclipse"        --> doShift "A"
                         , className =? "Rhythmbox"      --> doShift "9"
                         , className =? "Do"             --> doIgnore
                         , className =? "Thunderbird"    --> doShiftView "8"
                         , className =? "MPlayer"        --> doShiftView "7"
                         ],

    logHook            = myDzenLogHook workspaceNames hookedDzens
  }

doView workspace = doF (Stk.view workspace)
doShiftView workspace = doShift workspace <+> doView workspace

