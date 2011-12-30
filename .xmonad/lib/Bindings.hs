module Bindings(myKeyBindings, myMouseBindings) where

import XMonad
import XMonad.Layout.LayoutCombinators 
import XMonad.Hooks.ManageDocks
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)

import qualified XMonad.StackSet as Stk

import Graphics.X11.ExtraTypes.XF86

import Data.Map (fromList)

midRect = Stk.RationalRect (1/4) (1/4) (1/2) (1/2)

myKeyBindings conf@(XConfig {XMonad.modMask = modm}) = fromList $
  [ ((modm, xK_q     ), spawn "xmonad --restart") 
  , ((shmd, xK_q     ), spawn "xmonad --recompile; xmonad --restart")

  --focused window
  , ((shmd, xK_c     ), kill)
  , ((alt , xK_F4    ), kill)
  , ((modm, xK_n     ), refresh)

  --move focused window, keep focus on it
  , ((shmd, xK_Return), windows Stk.swapMaster)
  , ((shmd, xK_j     ), windows Stk.swapDown)
  , ((shmd, xK_k     ), windows Stk.swapUp)

  --cycle focused window
  , ((modm, xK_m     ), windows Stk.focusMaster)
  , ((modm, xK_j     ), windows Stk.focusDown)
  , ((modm, xK_k     ), windows Stk.focusUp)
  , ((modm, xK_Tab   ), windows Stk.focusDown)
  , ((shmd, xK_Tab   ), windows Stk.focusUp)

  --layout
  , ((modm, xK_f     ), sendMessage ToggleStruts)
  , ((modm, xK_t     ), withFocused $ windows . Stk.sink) --tile window
  , ((modm, xK_u     ), withFocused $ windows . (flip Stk.float midRect))
  , ((modm, xK_a     ), sendMessage $ JumpToLayout "left")
  , ((modm, xK_s     ), sendMessage $ JumpToLayout "top")
  , ((modm, xK_d     ), sendMessage $ JumpToLayout "full")
  , ((modm, xK_space ), sendMessage NextLayout)
  , ((shmd, xK_space ), setLayout $ XMonad.layoutHook conf)
  , ((modm, xK_h     ), sendMessage Shrink)
  , ((modm, xK_l     ), sendMessage Expand)
  , ((modm, xK_comma ), sendMessage (IncMasterN 1))
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))

  , ((modm, xK_b     ), withFocused toggleBorder)

  , ((modm, xK_v     ), windows copyToAll) -- Copy focused win to all workspaces
  , ((shmd, xK_v     ), killAllOtherCopies) -- @@ Delete copies of focused win

  --shortcuts
  , ((none, xK_Print ), spawn "$HOME/bin/scrot-bag")
  , ((modm, xK_p     ), spawn "gnome-do")
  , ((alt , xK_F2    ), spawn "gnome-do")

  , ((supr, xK_c     ), spawn "$HOME/bin/fcronjob co toggle")
  , ((supr, xK_t     ), spawn "$HOME/bin/fcronjob te toggle")

  , ((supr, xK_s     ), spawn "sleep 1; $HOME/bin/screenOff") --a la laptop lid closed
  , ((supr, xK_n     ), spawn "xcalib -i -a") --invert colors

  , ((none, xf86think), spawn "gnome-terminal")
  , ((ctrl, xf86think), spawnTerm "ghci")

  , ((none, xf86mic  ), spawn "$HOME/bin/pulse-mute microphone toggle")

  , ((alct, volUp    ), spawn "$HOME/bin/pulse-raise-volume -f")
  , ((alt,  volUp    ), spawn "$HOME/bin/pulse-raise-volume")
  , ((alt,  volDown  ), spawn "$HOME/bin/pulse-lower-volume")

  , ((alt , xK_Menu  ), spawn "$HOME/bin/nautilusDesktop toggle")
  , ((ctrl, xK_Menu  ), spawn "$HOME/bin/touchClick toggle")

  , ((supr, xK_F1    ), spawn "sudo cpu-set ondemand")
  , ((supr, xK_F2    ), spawn "sudo cpu-set conservative")
  , ((supr, xK_F3    ), spawn "sudo cpu-set powersave")
  , ((supr, xK_F4    ), spawn "sudo cpu-set performance")

  , ((supr, xK_1     ), spawn "sudo wauto -r; sudo wauto -c")
  , ((supr, xK_2     ), spawn "sudo wconnect -d; sudo n900-tether off")
  , ((supr, xK_3     ), spawn "sudo n900-tether on")

  , ((alct, xK_space ), spawn "gnome-terminal -x htop")

  , ((supr, xK_space ), spawn "$HOME/bin/klomp-cmd pause")
  , ((supr, xK_z     ), spawn "$HOME/bin/klomp-cmd prev")
  , ((supr, xK_x     ), spawn "$HOME/bin/klomp-cmd next")
  , ((sush, xK_z     ), spawn "$HOME/bin/klomp-cmd seek -10")
  , ((sush, xK_x     ), spawn "$HOME/bin/klomp-cmd seek 10")
  , ((supr, xK_r     ), spawn "$HOME/bin/klomp-cmd reset")

  , ((alct, xf86back ), spawn "$HOME/bin/rotate counterclockwise")
  , ((alct, xf86fwd  ), spawn "$HOME/bin/rotate clockwise")

  , ((alct, xK_f     ), spawn "firefox")
  , ((alct, xK_e     ), spawn "$HOME/bin/eclipse")
  , ((alct, xK_s     ), spawn "$HOME/bin/squirrel")
  ]

  ++
  -- mod-[1..9], Switch to workspace N
  [((modm, key), windows $ Stk.greedyView wkspc)
      | (key, wkspc) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)]
  ++
  -- mod-shift-[1..9], Move client to workspace N
  [((shmd, key), windows $ Stk.shift wkspc)
      | (key, wkspc) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)]
  ++
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  [((modm, key), screenWorkspace sc >>= flip whenJust (windows . Stk.view))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]
  ++
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((shmd, key), screenWorkspace sc >>= flip whenJust (windows . Stk.shift))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]
  where
    alt = mod1Mask
    ctrl = controlMask
    supr = mod4Mask
    shft = shiftMask
    shmd = modm .|. shft
    sush = supr .|. shft
    alct = alt .|. ctrl
    none = 0
    volUp = xF86XK_AudioRaiseVolume
    volDown = xF86XK_AudioLowerVolume
    xf86think = xF86XK_Launch1
    xf86mic = xF86XK_Launch2
    xf86back = xF86XK_Back
    xf86fwd = xF86XK_Forward
    hbin = "$HOME/bin/"
    spawnTerm cmd = spawn $ "gnome-terminal -x " ++ cmd
 
myMouseBindings (XConfig {XMonad.modMask = modm}) = fromList $
  -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                     >> windows Stk.shiftMaster))
 
  -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), (\w -> focus w >> windows Stk.shiftMaster))
 
  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                     >> windows Stk.shiftMaster))
  ]

