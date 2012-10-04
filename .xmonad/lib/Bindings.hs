module Bindings(myKeyBindings, myMouseBindings) where

import XMonad
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import FloatKeys

import qualified XMonad.StackSet as Stk

import Graphics.X11.ExtraTypes.XF86

import Data.Map (fromList)

midRect = Stk.RationalRect (1/4) (1/4) (1/2) (1/2)

--http://xmonad.org/xmonad-docs/X11/src/Graphics-X11-Types.html
myKeyBindings conf@(XConfig {XMonad.modMask = modm}) = fromList $
  [ ((modm, xK_q     ), spawn "xmonad --restart") 
  , ((shmd, xK_q     ), spawn "xmonad --recompile; xmonad --restart")

  , ((none, power    ), spawn "off g")
  , ((modm, xK_Escape), spawn "off g")

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

  --move/resize floating windows
  , ((ctrl, xK_Up    ), withFocused (keysMoveWindow (0,-20)))
  , ((ctrl, xK_Down  ), withFocused (keysMoveWindow (0,20)))
  , ((ctrl, xK_Left  ), withFocused (keysMoveWindow (-20,0)))
  , ((ctrl, xK_Right ), withFocused (keysMoveWindow (20,0)))
  , ((alct, xK_Up    ), withFocused (keysResizeWindow (0,-20) (0,0)))
  , ((alct, xK_Down  ), withFocused (keysResizeWindow (0,20) (0,0)))
  , ((alct, xK_Left  ), withFocused (keysResizeWindow (-20,0) (0,0)))
  , ((alct, xK_Right ), withFocused (keysResizeWindow (20,0) (0,0)))

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

  , ((modm, xK_v     ), windows copyToAll) -- Put on all workspaces
  , ((shmd, xK_v     ), killAllOtherCopies) -- Remove from other workspaces

  --shortcuts
  , ((none, xf86think), spawn "term")
  , ((none, reload   ), spawn "term")
  , ((alt,  xK_F2    ), spawn "term")
  , ((alt,  xf86think), spawn "LaunchTerm.hs")
  , ((ctrl, xf86think), spawn "term ghci")

  , ((none, xK_Print ), spawn "scrot-bag")

  , ((ctrl, xK_F9    ), spawn "pi -vnc")

  , ((ctrl, xK_F12   ), spawn "n9 -s lock")
  , ((ctsh, xK_F12   ), spawn "n9 -s dontgosleep")
  , ((ctrl, xK_F11   ), spawn "n9 -vnc")
  , ((ctrl, xK_F10   ), spawn "n9 -vnc -rotate 0")
  , ((ctsu, xK_space ), spawn "n9u -b klomp-cmd pause")
  , ((ctsu, xK_z     ), spawn "n9u -b klomp-cmd prev")
  , ((ctsu, xK_x     ), spawn "n9u -b klomp-cmd next")
  , ((ctsu, xK_b     ), spawn "n9u -b klomp-cmd playlist books")
  , ((ctsu, xK_5     ), spawn "n9u -b klomp-cmd volume 10 1")
  , ((ctsu, xK_6     ), spawn "n9u -b klomp-cmd volume 25 1")
  , ((ctsu, xK_7     ), spawn "n9u -b klomp-cmd volume 75 1")
  , ((ctsu, xK_8     ), spawn "n9u -b klomp-cmd volume 100 1")
  , ((ctsu, xK_9     ), spawn "n9u -b klomp-cmd volume -1 0")
  , ((ctsu, xK_0     ), spawn "n9u -b klomp-cmd volume +1 0")
  , ((chsu, xK_z     ), spawn "n9u -b klomp-cmd seek -10")
  , ((chsu, xK_x     ), spawn "n9u -b klomp-cmd seek 10")
  , ((chsu, xK_a     ), spawn "n9u -b klomp-cmd seek -60")
  , ((chsu, xK_s     ), spawn "n9u -b klomp-cmd seek 60")

  , ((supr, xK_space ), spawn "klomp-cmd pause")
  , ((supr, xK_z     ), spawn "klomp-cmd prev")
  , ((supr, xK_x     ), spawn "klomp-cmd next")
  , ((supr, xK_b     ), spawn "klomp-cmd playlist books")
  , ((sush, xK_z     ), spawn "klomp-cmd seek -10")
  , ((sush, xK_x     ), spawn "klomp-cmd seek 10")
  , ((sush, xK_a     ), spawn "klomp-cmd seek -60")
  , ((sush, xK_s     ), spawn "klomp-cmd seek 60")

  , ((supr, xK_c     ), spawn "fcronjob co toggle")
  , ((supr, xK_t     ), spawn "fcronjob te toggle")

  , ((supr, xK_s     ), spawn "sleep 1; screenOff") --monitor off
  , ((shmd, xK_s     ), spawn "sleep 1; screenOff") --monitor off
  , ((supr, xK_n     ), spawn "xcalib -i -a") --invert colors

  , ((none, xf86mic  ), spawn "pulse-vol microphone toggle")
  , ((none, xf86mute ), spawn "pulse-vol speaker toggle")

  , ((ctrl, xK_Home  ), spawn "brightness up")
  , ((ctrl, xK_End   ), spawn "brightness down")
  , ((none, brightUp ), spawn "brightness system")  --let system change it
  , ((none, brightDn ), spawn "brightness system")  --let system change it
  
  , ((ctrl, pgUp     ), spawn "led thinklight") --a synonym for Fn+PgUp

  , ((supr, volUp    ), spawn "speaker toggle; klomp-cmd restart")
  , ((none, volUp    ), spawn "pulse-vol +6 100")
  , ((none, volDown  ), spawn "pulse-vol -6 100")
  , ((alt,  volUp    ), spawn "pulse-vol +6 150")
  , ((alt,  volDown  ), spawn "pulse-vol -6 150")
  , ((alct, volUp    ), spawn "pulse-vol +6 300")
  , ((alct, volDown  ), spawn "pulse-vol -6 300")

  , ((ctrl, xK_Menu  ), spawn "touchClick toggle")

  , ((supr, xK_F1    ), spawn "sudo cpu-set ondemand 800 2201")
  , ((supr, xK_F2    ), spawn "sudo cpu-set ondemand 800 1400")
  , ((supr, xK_F3    ), spawn "sudo cpu-set ondemand 800 800")
  , ((supr, xK_F4    ), spawn "sudo cpu-set ondemand 2201 2201")
  , ((shmd, xK_F1    ), spawn "sudo cpu-set ondemand 800 2201")
  , ((shmd, xK_F2    ), spawn "sudo cpu-set ondemand 800 1400")
  , ((shmd, xK_F3    ), spawn "sudo cpu-set ondemand 800 800")
  , ((shmd, xK_F4    ), spawn "sudo cpu-set ondemand 2201 2201")

  , ((supr, xK_1     ), spawn "sudo wauto")
  , ((supr, xK_2     ), spawn "sudo wconnect -d; sudo tether off; sudo wired off")
  , ((supr, xK_3     ), spawn "sudo tether on")
  , ((supr, xK_4     ), spawn "sudo wired on")

  , ((alct, xK_space ), spawn "term htop")

  , ((alct, xf86back ), spawn "rotate counterclockwise")
  , ((alct, xf86fwd  ), spawn "rotate clockwise")

  , ((alct, xK_f     ), spawn "firefox")
  , ((alct, xK_c     ), spawn "chromium-browser")
  , ((alct, xK_t     ), spawn "transmission-gtk")
  , ((alct, xK_e     ), spawn "eclipse")
  , ((alct, xK_s     ), spawn "stepmania -w")
  , ((alct, xK_i     ), spawn "stepmania -i")
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
    ctsh = ctrl .|. shft
    ctsu = ctrl .|. supr
    chsu = shft .|. ctrl .|. supr
    none = 0
    pgUp = xK_Prior
    pgDn = xK_Next
    brightUp = xF86XK_MonBrightnessUp
    brightDn = xF86XK_MonBrightnessDown
    volUp = xF86XK_AudioRaiseVolume
    power = xF86XK_PowerOff
    volDown = xF86XK_AudioLowerVolume
    reload = xF86XK_Reload
    xf86think = xF86XK_Launch1
    xf86mic = xF86XK_Launch2
    xf86mute = xF86XK_AudioMute
    xf86back = xF86XK_Back
    xf86fwd = xF86XK_Forward
 
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

