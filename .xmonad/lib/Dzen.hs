module Dzen(spawnHookedDzens, spawnUnhookedDzens, myDzenLogHook) where

import DzenXinerama
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run


myDzenLogHook workspaceNames dzens = dzenXineramaLogHook $
      map (\dzen -> (myDzenPP workspaceNames) {ppOutput = hPutStrLn dzen}) dzens

spawnUnhookedDzens = do 
  nScreens <- countScreens
  mapM (\n -> dzenBarExtras n) [1..nScreens]

spawnHookedDzens = do
  nScreens <- countScreens
  mapM (\((x, y), n) -> dzenBarXmonad x y n) $
     zip pos [1..nScreens]


--position of dzen bar by xinerama screen {addtl screen default is (0,0)}
pos = [] ++ repeat (0,0)
height = 36
font = "Inconsolata-14"

dzenExec = "sleep 1; dzen2"
dzenExtrasExec = "sleep 2; $HOME/.dzen2/launchers/main"

myDzenPP workspaceNames = dzenPP
  { ppCurrent  = \x -> "^bg(#cccccc)^fg(black) " ++ x ++ " " ++ "^r(16)" ++
                 (box "red" 43 height 3) ++ blkspc
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
dzenFlags = ""
  ++ " -e " ++ "'onstart=raise'"
  ++ " -fg " ++ "'#a8a3f7'"
  ++ " -bg " ++ "'#3f3c6d'"
  ++ " -h " ++ show height
  ++ " -fn " ++ font

dzenBarXmonad x y screen = spawnPipe $ dzenExec ++ dzenFlags ++ posFlags
 where posFlags = ""
          ++ " -xs " ++ show screen
          ++ " -x " ++ show x
          ++ " -y " ++ show y
          ++ " -ta " ++  "l"

dzenBarExtras screen = spawnPipe $ dzenExtrasExec ++ dzenFlags ++ posFlags
 where posFlags = ""
          ++ " -xs " ++ show screen

