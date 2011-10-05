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
width = 18 + 36
height = 36
space = 4
font = "Inconsolata-14"

alwaysShown = ["A", "B", "D", "G"]

dzenExec = "sleep 2; dzen2"
dzenExtrasExec = "sleep 1; $HOME/.dzen2/launchers/main"

myDzenPP workspaceNames = dzenPP
  { ppCurrent         = current
  , ppVisible         = emptyWs "#999999"
  , ppHidden          = hidden
  , ppHiddenNoWindows = empty
  , ppUrgent          = \x -> emptyWs "red" x
  , ppWsSep           = blkspc
  , ppSep             = blkspc
  , ppLayout          = dzenColor "black" "yellow" .
                        (\ x -> case x of
                         "left" -> "[]="
                         "top"  -> "TTT"
                         "full" -> "[ ]"
                         _      -> x
                        )
  , ppTitle           = ("^bg(#316c80) " ++) . dzenEscape . shorten 30
  }
  where
   current wsName = ""
     ++ wsMarkup "#cccccc" wsName
     ++ box "red" width height 3
   hidden wsName  = emptyWs "#cccccc" wsName
   empty wsName   = if elem wsName alwaysShown then hidden wsName else ""
   img wsName = "^i(/home/wolke/.xmonad/workspace-images/" ++ wsName ++ ".xbm)"

   wsMarkup bg wsName = col "black" bg $ ""
     ++ " " ++ wsName ++ (img wsName)  --approximately 72px....
   
   col fg bg markup = ""
     ++ "^fg(" ++ fg ++ ")" ++ "^bg(" ++ bg ++ ")"
     ++ markup
     ++ "^bg()^fg()"
   emptyWs bg wsName = ""
     ++ wsMarkup bg wsName
     ++ "^fg(" ++ bg ++ ")"
     ++ clickRect width height (clickCmd wsName)
     ++ "^fg()"
   blkspc = ""
     ++ "^fg(black)"
     ++ "^r(" ++ show space ++ "x" ++ show height ++ ")"
     ++ "^fg()"
   clickRect w h cmd = ""
     ++ "^p(-" ++ show width ++ ")"
     ++ "^ca(1," ++ cmd ++ ")"
       ++ "^ib(1)"
       ++ "^ro(" ++ show w ++ "x" ++ show h ++ ")"
       ++ "^ib(0)"
     ++ "^ca()"
   clickCmd wsName = "xdotool key alt+" ++ (show $ wsKey wsName)
   wsKey wsName = 1 + (wsIndex wsName workspaceNames)
   wsIndex wsName (ws:wss) | ws == wsName = 0
                           | otherwise    = 1 + wsIndex wsName wss
   box color w h px = ""
     ++ "^ib(1)^fg(" ++ color ++ ")"
     ++ (boxh w w h px)
     ++ "^fg()^ib(0)"
     where boxh x w h px | px > 1  = ""
                          ++ boxh x w h 1
                          ++ boxh (w-1) (w-2) (h-2) (px-1)
                          ++ "^p(1)"
                       | px == 1 = ""
                          ++ "^p(-" ++ show x ++ ")"
                          ++ "^ro(" ++ show w ++ "x" ++ show h ++ ")"

dzenFlags = ""
  ++ " -e " ++ "'onstart=lower'"
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

