import XMonad hiding ( (|||) ) --use LayoutCombinators JumpToLayout |||
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.Named
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Actions.DeManage (demanage)
import XMonad.Util.Font (encodeOutput)
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as Stk
import Data.Monoid
import Data.Function (on)
import Data.Ord (comparing)
import Data.Map (fromList)
import Data.Maybe (isJust, catMaybes)
import Data.List (sortBy, intercalate)

import Control.Monad (zipWithM_)

import System.Exit

--position of dzen bar by xinerama screen {addtl screen default is (0,0)}
pos = [] ++ repeat (0,0)
xtraPos = [] ++ repeat (500,0)
--extras on dzen bar by xinerama screen {addtl screen default is False}
height = 24

workspaceNames = ["A", "B", "D", "G", "5", "6", "7", "8", "9"]

dzenExtras = "$HOME/.dzen2/launchers/main"

main = do
  nScreens <- countScreens
  dzenKill <- spawn "killall dzen2"
  --no update hook, updates are up to dzen-extras
  xtraBars <- mapM (\((x, y), n) -> dzenBar dzenExtras "r" x y n) $
              zip xtraPos [1..nScreens]
  --update hooks
  bars <- mapM (\((x, y), n) -> dzenBar "sleep 0.3; dzen2" "l" x y n) $
          zip pos [1..nScreens]
  xmonad $ defaultConfig {
    terminal           = "gnome-terminal",
    focusFollowsMouse  = False,
    borderWidth        = 2,
    modMask            = mod1Mask,
    workspaces         = workspaceNames,
    normalBorderColor  = "#dddddd",
    focusedBorderColor = "#ff0000",

    keys               = myKeys,
    mouseBindings      = myMouseBindings,
  
    layoutHook         =
      (named "left" $ avoidStruts $          Tall 1 (3/100) (55/100)) |||
      (named "top"  $ avoidStruts $ Mirror $ Tall 1 (3/100) (55/100)) |||
      (named "full" $ avoidStruts $          Full),
    manageHook         = windowRules,
    handleEventHook    = mempty,
    startupHook        = return (),

    logHook            = dzenLogHook $
      map (\bar -> myDzenPP {ppOutput = hPutStrLn bar}) bars
  }

myDzenPP = dzenPP
  { ppCurrent  = \x -> "^bg(#cccccc)^fg(black) " ++ x ++ " " ++ "^r(16)" ++
                 (box "red" 37 height 3) ++ blkspc
  , ppVisible  = emptyWs "#999999"
  , ppHidden = \x -> emptyWs "#cccccc" x
  , ppHiddenNoWindows =
    (\x -> case x of
      "A" -> emptyWs "#cccccc" x
      "B" -> emptyWs "#cccccc" x
      "D" -> emptyWs "#cccccc" x
      "G" -> emptyWs "#cccccc" x
      _   -> ""
    )
  , ppUrgent   = dzenColor "red" "yellow" . dzenStrip
  , ppWsSep    = ""
  , ppSep      = ""
  , ppLayout   = dzenColor "black" "yellow" .
    (\ x -> case x of
      "left" -> "[]="
      "top"  -> "TTT"
      "full" -> "[ ]"
      _      -> x
    )
  , ppTitle    = ("^bg(#316c80) " ++) . dzenEscape . shorten 30
  }
  where
   emptyWs bg wsName = clickSwitch wsName
     (dzenColor "black" bg $ pad wsName ++ "^r(16)") ++ blkspc
   blkspc = "^bg(black) ^bg()"
   wsIndex wsName (ws:wss) | ws == wsName = 0
                           | otherwise    = 1 + wsIndex wsName wss
   clickSwitch wsName dzenMarkup = 
     "^ca(1,xdotool key alt+" ++
       (show $ 1+wsIndex wsName workspaceNames) ++ ")" ++
       dzenMarkup ++
     "^ca()"
   box color w h px = go w w h px
     where go x w h px | px > 1  = go x w h 1
                                   ++ go (w-1) (w-2) (h-2) (px-1)
                                   ++ "^p(1)"
                       | px == 1 = "^p(-" ++ show x ++ ")"
                                   ++ "^fg(" ++ color ++ ")"
                                   ++ "^ib(1)"
                                   ++ "^ro(" ++ show w ++ "x" ++ show h ++ ")"
                                   ++ "^ib(0)"

dzenBar cmd align x y screen = spawnPipe $ cmd ++ flags
 where flags = ""
          ++ " -e " ++ "'onstart=lower'"
          ++ " -fg " ++ "'#a8a3f7'"
          ++ " -bg " ++ "'#3f3c6d'"
          ++ " -xs " ++ show screen
          ++ " -x " ++ show x
          ++ " -y " ++ show y
          ++ " -h " ++ show height
          ++ " -ta " ++  align


windowRules = composeAll
    [ className =? "Gnome-panel"    --> doIgnore
    , className =? "Do"             --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

midRect = Stk.RationalRect (1/4) (1/4) (1/2) (1/2)

myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
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
  , ((modm, xK_b     ), sendMessage ToggleStruts)
  , ((modm, xK_t     ), withFocused $ windows . Stk.sink) --tile window
  , ((modm, xK_f     ), withFocused $ windows . (flip Stk.float midRect))
  , ((modm, xK_a     ), sendMessage $ JumpToLayout "left")
  , ((modm, xK_s     ), sendMessage $ JumpToLayout "top")
  , ((modm, xK_d     ), sendMessage $ JumpToLayout "full")
  , ((modm, xK_space ), sendMessage NextLayout)
  , ((shmd, xK_space ), setLayout $ XMonad.layoutHook conf)
  , ((modm, xK_h     ), sendMessage Shrink)
  , ((modm, xK_l     ), sendMessage Expand)
  , ((modm, xK_comma ), sendMessage (IncMasterN 1))
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))

  , ((modm, xK_g     ), withFocused toggleBorder)

  , ((modm, xK_v     ), windows copyToAll) -- Copy focused win to all workspaces
  , ((shmd, xK_v     ), killAllOtherCopies) -- @@ Delete copies of focused win
  , ((modm, xK_c     ), withFocused demanage)

  --shortcuts
  , ((none, xK_Print ), spawn "gnome-screenshot")
  , ((modm, xK_p     ), spawn "gnome-do")
  , ((alt , xK_F2    ), spawn "gnome-do")

  , ((none, xf86think), spawn "gnome-terminal")
  , ((alt,  xf86think), spawnTerm "$HOME/bin/cmdselect")
  , ((ctrl, xf86think), spawnTerm "ghci")

  , ((alt , xK_Menu  ), spawn "$HOME/bin/nautilusDesktop toggle")
  , ((ctrl, xK_Menu  ), spawn "$HOME/bin/touchClick toggle")

  , ((alct, xK_space ), spawn "gnome-system-monitor")

  , ((supr, xK_space ), spawn "$HOME/bin/rc-start --play-pause")
  , ((supr, xK_z     ), spawn "$HOME/bin/rc --previous")
  , ((supr, xK_x     ), spawn "$HOME/bin/rc --next")

  , ((alct, xf86back ), spawn "$HOME/bin/rotate counterclockwise")
  , ((alct, xf86fwd  ), spawn "$HOME/bin/rotate clockwise")

  , ((alct, xK_f     ), spawn "firefox")
  , ((alct, xK_e     ), spawn "$HOME/bin/eclipse")
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
    alct = alt .|. ctrl
    none = 0
    xf86think = xF86XK_Launch1
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


------------------------------------------------------------------------
-- dzen xinerama visible/active workspace support
dzenLogHook pps = do
 screens <- (sortBy (compare `on` Stk.screen) . Stk.screens)
            `fmap`
            gets windowset
 zipWithM_ dynamicLogWithPPScreen screens pps


dynamicLogWithPPScreen screen pp =
  dynamicLogStringScreen screen pp >>= io . ppOutput pp

dynamicLogStringScreen screen pp = do

  winset <- gets windowset
  urgents <- readUrgents
  sortF <- ppSort pp

  -- layout description
  let ld = description . Stk.layout . Stk.workspace $ screen

  -- workspace list
  let ws = pprWindowSetScreen screen sortF urgents pp winset

  -- window title
  wt <- maybe (return "") (fmap show . getName) $ focusedWindow screen

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (`catchX` return Nothing) $ ppExtras pp

  return $ encodeOutput . sepBy (ppSep pp) . ppOrder pp $
             [ ws
             , ppLayout pp ld
             , ppTitle pp wt
             ]
             ++ catMaybes extras

pprWindowSetScreen screen sortF urgents pp s =
  sepBy (ppWsSep pp) . map fmt . sortF $ Stk.workspaces s
    where this = Stk.tag . Stk.workspace $ screen
          visibles = map (Stk.tag . Stk.workspace) 
                         (Stk.current s : Stk.visible s)

          fmt w = printer pp (Stk.tag w)
              where printer | Stk.tag w == this = ppCurrent
                            | Stk.tag w `elem` visibles = ppVisible
                            | any (\x -> maybe False (== Stk.tag w)
                                                     (Stk.findTag x s))
                                  urgents = \ppC -> ppUrgent ppC . ppHidden ppC
                            | isJust (Stk.stack w) = ppHidden
                            | otherwise = ppHiddenNoWindows

focusedWindow = maybe Nothing (return . Stk.focus) . Stk.stack . Stk.workspace

sepBy :: String -> [String] -> String
sepBy sep = intercalate sep . filter (not . null)


