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

  , ((none, power    ), spawn "$HOME/bin/off g")
  , ((modm, xK_Escape), spawn "$HOME/bin/off g")

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

  , ((ctrl, xK_F12   ), spawn "$HOME/bin/n9 -s lock")
  , ((ctsh, xK_F12   ), spawn "$HOME/bin/n9 -s screenStayOnDaemon")
  , ((ctrl, xK_F11   ), spawn "$HOME/bin/n9 -vnc")
  , ((ctrl, xK_F10   ), spawn "$HOME/bin/n9 -vnc -rotate 0")
  , ((ctrl, xK_F9    ), spawn "termcmd n9 -s")
  , ((ctsu, xK_space ), spawn "n9 -s udo klomp-cmd pause")
  , ((ctsu, xK_z     ), spawn "n9 -s udo klomp-cmd prev")
  , ((ctsu, xK_x     ), spawn "n9 -s udo klomp-cmd next")
  , ((chsu, xK_z     ), spawn "n9 -s udo klomp-cmd seek -10")
  , ((chsu, xK_x     ), spawn "n9 -s udo klomp-cmd seek 10")
  , ((ctsu, xK_r     ), spawn "n9 -s udo klomp-cmd reset")


  --shortcuts
  , ((none, xf86think), spawn "term")
  , ((alt,  xf86think), spawn "$HOME/bin/LaunchTerm.hs")
  , ((ctrl, xf86think), spawnTerm "ghci")

  , ((none, xK_Print ), spawn "$HOME/bin/scrot-bag")

  , ((supr, xK_c     ), spawn "$HOME/bin/fcronjob co toggle")
  , ((supr, xK_t     ), spawn "$HOME/bin/fcronjob te toggle")

  , ((supr, xK_s     ), spawn "sleep 1; $HOME/bin/screenOff") --monitor off
  , ((supr, xK_n     ), spawn "xcalib -i -a") --invert colors

  , ((none, xf86mic  ), spawn "$HOME/bin/pulse-vol microphone toggle")
  , ((none, xf86mute ), spawn "$HOME/bin/pulse-vol speaker toggle")

  , ((ctrl, xK_Home  ), spawn "$HOME/bin/brightness up")
  , ((ctrl, xK_End   ), spawn "$HOME/bin/brightness down")
  , ((none, brightUp ), spawn "rm $HOME/.brightness")  --remove autoreset file
  , ((none, brightDn ), spawn "rm $HOME/.brightness")  --remove autoreset file
  
  , ((ctrl, pgUp     ), spawn "led thinklight") --a synonym for Fn+PgUp

  , ((none, volUp    ), spawn "$HOME/bin/pulse-vol +6 100")
  , ((none, volDown  ), spawn "$HOME/bin/pulse-vol -6 100")
  , ((alt,  volUp    ), spawn "$HOME/bin/pulse-vol +6 150")
  , ((alt,  volDown  ), spawn "$HOME/bin/pulse-vol -6 150")
  , ((alct, volUp    ), spawn "$HOME/bin/pulse-vol +6 300")
  , ((alct, volDown  ), spawn "$HOME/bin/pulse-vol -6 300")

  , ((ctrl, xK_Menu  ), spawn "$HOME/bin/touchClick toggle")

  , ((supr, xK_F1    ), spawn "sudo cpu-set ondemand")
  , ((supr, xK_F2    ), spawn "sudo cpu-set conservative")
  , ((supr, xK_F3    ), spawn "sudo cpu-set powersave")
  , ((supr, xK_F4    ), spawn "sudo cpu-set performance")

  , ((supr, xK_1     ), spawn "sudo wauto")
  , ((supr, xK_2     ), spawn "sudo wconnect -d; sudo tether off; sudo wired off")
  , ((supr, xK_3     ), spawn "sudo tether on")
  , ((supr, xK_4     ), spawn "sudo wired on")

  , ((alct, xK_space ), spawn "term -e htop")

  , ((supr, xK_space ), spawn "klomp-cmd pause")
  , ((supr, xK_z     ), spawn "klomp-cmd prev")
  , ((supr, xK_x     ), spawn "klomp-cmd next")
  , ((sush, xK_z     ), spawn "klomp-cmd seek -10")
  , ((sush, xK_x     ), spawn "klomp-cmd seek 10")
  , ((supr, xK_r     ), spawn "klomp-cmd reset")

  , ((alct, xf86back ), spawn "$HOME/bin/rotate counterclockwise")
  , ((alct, xf86fwd  ), spawn "$HOME/bin/rotate clockwise")

  , ((alct, xK_f     ), spawn "firefox")
  , ((alct, xK_e     ), spawn "$HOME/bin/eclipse")
  , ((alct, xK_s     ), spawn "$HOME/bin/stepmania")
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
    xf86think = xF86XK_Launch1
    xf86mic = xF86XK_Launch2
    xf86mute = xF86XK_AudioMute
    xf86back = xF86XK_Back
    xf86fwd = xF86XK_Forward
    spawnTerm cmd = spawn $ "term -e " ++ cmd
 
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

