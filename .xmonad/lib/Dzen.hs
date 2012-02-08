module Dzen(spawnHookedDzens, spawnUnhookedDzens, myDzenLogHook) where

import DzenXinerama
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import IO (Handle())

fgColor = "#a8a3f7"
bgColor = "#3f3c6d"

myDzenLogHook home workspaceNames dzens = dzenXineramaLogHook $
      map (\dzen -> (myDzenPP home workspaceNames) {ppOutput = hPutStrLn dzen}) dzens

spawnUnhookedDzens = do 
  nScreens <- countScreens
  mapM (\n -> dzenBarExtras n) [1..nScreens] :: IO [Handle]

spawnHookedDzens = do
  nScreens <- countScreens
  mapM (\((x, y), n) -> dzenBarXmonad x y n) $
     zip pos [1..nScreens] :: IO [Handle]


--position of dzen bar by xinerama screen {addtl screen default is (0,0)}
pos = [] ++ repeat (0,0)
width = 18 + 36
height = 36
space = 4
font = "Inconsolata-14"

alwaysShown = ["A", "B", "D", "G"]

dzenExec = "sleep 2; dzen2"
dzenExtrasExec = "sleep 1; $HOME/.dzen2/launchers/main"

myDzenPP home workspaceNames = dzenPP
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
  , ppTitle           = clickWrap 1 "$HOME/bin/kb" .
                        ("^bg(#316c80) " ++) .
                        dzenEscape .
                        shorten 30
  }
  where
   current wsName = ""
     ++ wsMarkup "#cccccc" wsName
     ++ box "red" width height 3
   hidden wsName  = emptyWs "#cccccc" wsName
   empty wsName   = if elem wsName alwaysShown
                    then emptyWs bgColor wsName
                    else ""
   img wsName = "^i(" ++ home ++ "/.xmonad/workspace-images/" ++ wsName ++ ".xbm)"

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
   clickWrap btn cmd markup = ""
     ++ "^ca(" ++ (show btn) ++ "," ++ cmd ++ ")"
       ++ markup
     ++ "^ca()"
   clickRect w h cmd = ""
     ++ "^p(-" ++ show width ++ ")"
     ++ "^fn(monospace-100)" --clickable area is function of fontsize {f%#kers}
     ++ (clickWrap 1 cmd $ ""
       ++ "^ib(1)"
       ++ "^ro(" ++ show w ++ "x" ++ show h ++ ")"
       ++ "^ib(0)")
     ++ "^fn()"
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
  ++ " -fg " ++ "'" ++ fgColor ++ "'"
  ++ " -bg " ++ "'" ++ bgColor ++ "'"
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

