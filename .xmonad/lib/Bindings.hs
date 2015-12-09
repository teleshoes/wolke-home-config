module Bindings where
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Actions.FloatKeys (keysMoveWindow, keysResizeWindow)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.SinkAll
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import XMonad.StackSet hiding (focus, workspaces, filter)
import qualified XMonad.StackSet as SS

import Control.Arrow (first)
import qualified Data.Foldable as F
import Data.List (find, intercalate, isInfixOf, transpose)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Data.Maybe
import System.IO.Error (catchIOError)

import Bindings.Keys
import Bindings.Writer

main = putStr . prettyBindingsIndented $ keyBinds testConfig

tryWriteKeyBindingsPrettyCache file = writeKeyBindingsPrettyCache file `catchIOError` print
writeKeyBindingsPrettyCache = flip writeFile $ cols 4 $ lines $
    renameWorkspaces $ prettyBindingsFlat $ keyBinds testConfig

tryWriteKeyBindingsCache file = writeKeyBindingsCache file `catchIOError` print
writeKeyBindingsCache = flip writeFile $
    renameWorkspaces $ prettyBindingsFlatHex $ keyBinds testConfig

renameWorkspaces = unlines . map rename . lines
  where
    rename line
      | "\"Workspaces\"" `isInfixOf` line
      , Just (old, new) <- findWorkspaceName = intercalate (show new) $ splitOn old line
      | otherwise                            = line
      where
        findWorkspaceName = find ((`isInfixOf` line) . fst) $
            map (first $ show . show) $ zip [1..] workspaceNames

myMouseBindings = M.fromList . bwBindList . mouseBinds
myKeyBindings   = M.fromList . bwBindList . keyBinds

workspaceNames = ["A", "B", "D", "G"] ++ map show [5..9]
testConfig = defaultConfig{ layoutHook = Layout $ layoutHook defaultConfig
                          , workspaces = workspaceNames }

mouseOverlaps = bwFindOverlap $ mouseBinds testConfig
keyOverlaps   = bwFindOverlap $ keyBinds   testConfig

cols colCount lines = unlines $ map concat $ transpose cols
  where
    maxLen = maximum $ map length lines
    colSize = negate $ negate (length lines) `div` colCount
    cols = chunksOf colSize $ map pad lines
    pad line = take (maxLen+2) $ line ++ repeat ' '

infixr 0 #!, ##, #^, #>
a #! b = a # (spawn b :: X ())
a ## b = a # windows b
a #^ b = a # withFocused b
a #> b = a # sendMessage b

mouseBinds conf = "Mouse Bindings" @@ do
    "Move Window"   @@ mW button1 # select >=> mouseMoveWindow
    "Raise Window"  @@ mW button2 # void . select
    "Resize Window" @@ mW button3 # select >=> mouseResizeWindow
  where
    select w = focus w >> windows shiftMaster >> return w

keyBinds conf = "Key Bindings" @@ mapM_ ($ conf)
    [xmoKeys, shortcuts, windowKeys, layoutKeys, workspaceKeys]

xmoKeys conf = "XMonad" @@ do
    "Restart Taffybar"  @@ mCA   xK_Home #! "taffybar-restart"
    "Recompile Xmonad"  @@ mCA   xK_End  #! "xmonad-restart"
    "Edit Keys"         @@ mCA   xK_Del  #! "term vim ~/.xmonad/lib/Bindings.hs"


shortcuts conf = "Shortcuts" @@ do
    "off"               @@ [m_ xK_Power, mA xK_Esc] #! "off g"
    "term"              @@ m_    xK_Think#! "term"
    "term"              @@ m_    xK_Audio#! "term"
    "term"              @@ m_    xK_Tools#! "term"
    "term"              @@ m_    xK_Rfrsh#! "term"
    "term"              @@ mA    xK_F2   #! "term"
    "term auto-cwd"     @@ mA    xK_Think#! "term -acd"
    "ghci"              @@ mC    xK_Think#! "term ghci"
    "bgset --next"      @@ mW    xK_w    #! "bgset --next"
    "bgset --prev"      @@ mWS   xK_w    #! "bgset --prev"
    "screenshot"        @@ m_    xK_Print#! "scrot-bag ~/Desktop/Pictures/Screenshots"
    "Invert Colors"     @@ mW    xK_n    #! "xcalib -i -a"
    "Screen Off Idle"   @@ mW    xK_s    #! "sleep 0.5; screenOff"
    "Screen On/Off"     @@ mCW   xK_s    #! "screenpwr"
    "thinklight"        @@ mC    xK_PgUp #! "led thinklight"
    "touchclick toggle" @@ mC    xK_Menu #! "touchClick toggle"
    "htop"              @@ mCA   (xK ' ')#! "term htop"

    "Network"       @@ do
        "wauto"         @@ mW    xK_1    #! "sudo wauto"
        "off"           @@ mW    xK_2    #! "sudo wconnect -d; " ++
                                            "sudo tether off; " ++
                                            "sudo wired off"
        "tether"        @@ mW    xK_3    #! "sudo tether on"
        "wired"         @@ mW    xK_4    #! "sudo wired on"

    "Brightness"    @@ do
        "Up"            @@ mC    xK_Home #! "brightness up"
        "Down"          @@ mC    xK_End  #! "brightness down"
        "{system up}"   @@ m_    xK_BriUp#! "brightness system"
        "{system down}" @@ m_    xK_BriDn#! "brightness system"

    "Sound"         @@ do
        let [up,down] = map (++ "100/150/300") ["+","-"]
        up          @@  do m_    xK_VolUp#! "pulse-vol +6 100"
                           mA    xK_VolUp#! "pulse-vol +6 150"
                           mC    xK_VolUp#! "pulse-vol +6 300"
        down        @@  do m_    xK_VolDn#! "pulse-vol -6 100"
                           mA    xK_VolDn#! "pulse-vol -6 150"
                           mC    xK_VolDn#! "pulse-vol -6 300"
        "Toggle mute"   @@ m_    xK_Mute #! "pulse-vol speaker toggle"
        "Force mute"    @@ mA    xK_Mute #! "pulse-vol 0; pulse-vol speaker mute"
        "Mute Mic"      @@ m_    xK_Mic  #! "pulse-vol microphone toggle"
        "spkr switch"   @@ mW    xK_VolUp#! "speaker toggle; klomp-cmd restart"
        "nuc cec start" @@ mCS   xK_equal#! "nuc cec-cmd start"
        "nuc cec mute"  @@ mCS   xK_minus#! "nuc cec-cmd --av mute"
        "nuc cec volup" @@ mC    xK_equal#! "nuc cec-cmd --av vol +1"
        "nuc cec voldn" @@ mC    xK_minus#! "nuc cec-cmd --av vol -1"
        "nuc vol up"    @@ mCA   xK_VolUp#! "nuc pulse-vol +6 100"
        "nuc vol down"  @@ mCA   xK_VolDn#! "nuc pulse-vol -6 100"
        "nuc spkr cyc"  @@ mCAW  xK_VolUp#! "nuc speaker cycle; nuc klomp-cmd restart"

    "iguana IR"     @@ do
        "A/C power"     @@ mC    xK_F1   #! "nuc igcmd ac-power"
        "A/C mode"      @@ mC    xK_F3   #! "nuc igcmd ac-mode"

    "CEC power"     @@ do
        "TV pwr toggle" @@ mC    xK_F2   #! "nuc cec-cmd --tv togglestandby"
        "TV pwr toggle" @@ mCA   xK_Ins  #! "nuc cec-cmd --tv togglestandby"

    "CPU"           @@ do
        "100%"          @@ mW    xK_F1   #! "sudo intel-pstate -s max 100"
        "66%"           @@ mW    xK_F2   #! "sudo intel-pstate -s max 66"
        "33%"           @@ mW    xK_F3   #! "sudo intel-pstate -s max 33"
        "0%"            @@ mW    xK_F4   #! "sudo intel-pstate -s max 0"

    "Fan"           @@ do
        "auto"          @@ mW    xK_F5   #! "sudo fan auto"
        "fastest"       @@ mW    xK_F6   #! "sudo fan disengaged"
        "medium"        @@ mW    xK_F7   #! "sudo fan 4"
        "off"           @@ mW    xK_F8   #! "sudo fan 0"

    "feh"           @@ do
        "left"          @@ mA    xK_F11  #! "xdotool search --class feh key Left"
        "right"         @@ mA    xK_F12  #! "xdotool search --class feh key Right"

    "alarms"        @@ do
        "coffee!"       @@ mW    xK_c    #! "fcron-job-toggle co"
        "tea!"          @@ mW    xK_t    #! "fcron-job-toggle te"

    "Rotate Deasil/Widdershins" @@ do
                           mCA   xK_Fwd  #! "rotate deasil"
                           mCA   xK_Back #! "rotate widdershins"

    "Applications"  @@ do
        "kodi"          @@ mCA   xK_k    #! "kodi"
        "Firefox"       @@ mCA   xK_f    #! "firefox"
        "Firefox 24"    @@ mCAS  xK_f    #! "ff24"
        "Chrome"        @@ mCA   xK_c    #! "chromium --incognito"
        "tor"           @@ mCA   xK_v    #! "tor"
        "sabnzbd"       @@ mCA   xK_b    #! "sabnzbd"
        "Pidgin"        @@ mCA   xK_p    #! "pidgin"
        "Transmission"  @@ mCA   xK_t    #! "transmission-gtk"
        "Eclipse"       @@ mCA   xK_e    #! "eclipse"
        "FBreader"      @@ mCA   xK_r    #! "fbreader"
        "stepmania"     @@ mCA   xK_s    #! "stepmania"
        "stepmania -i"  @@ mCA   xK_i    #! "stepmania -i"
        "smbc"          @@ mCA   xK_m    #! "smbc"

    "nuc"           @@ do
        "vnc"           @@ mC    xK_F9   #! "nuc -vnc"
        "vnc"           @@ mCS   xK_F9   #! "nuc -vnc --x2vnc"
        "bgset --next"  @@ mCAW  xK_w    #! "nuc bgset --next"
        "bgset --prev"  @@ mCAWS xK_w    #! "nuc bgset --prev"

    "raspi"         @@ do
        "pi outlet"     @@ mC    xK_PgDn #! "pi outlet"

    "N9"            @@ do
        "lock"          @@ mC    xK_F12  #! "n9 -b lock"
        "dontgosleep"   @@ mCS   xK_F12  #! "n9 -s dontgosleep"
        "vnc landscape" @@ mC    xK_F11  #! "n9 -vnc"
        "vnc portrait"  @@ mC    xK_F10  #! "n9 -vnc -rotate 0"

    "Klomp"         @@ do
        "pause"       @@ mW    (xK ' ')#! "klomp-cmd pause"
        "prev"        @@ mW    xK_z    #! "klomp-cmd prev"
        "next"        @@ mW    xK_x    #! "klomp-cmd next"
        "books pl"    @@ mW    xK_b    #! "klomp-cmd playlist books"
        "seek -10"    @@ mWS   xK_z    #! "klomp-cmd seek -10"
        "seek 10"     @@ mWS   xK_x    #! "klomp-cmd seek 10"
        "seek -60"    @@ mWS   xK_a    #! "klomp-cmd seek -60"
        "seek 60"     @@ mWS   xK_s    #! "klomp-cmd seek 60"
        "save"        @@ mW    xK_v    #! "save-klomp"

    "Klomp nuc"     @@ do
        "pause"       @@ mCAW  (xK ' ')#! "nuc klomp-cmd pause"
        "prev"        @@ mCAW  xK_z    #! "nuc klomp-cmd prev"
        "next"        @@ mCAW  xK_x    #! "nuc klomp-cmd next"
        "books pl"    @@ mCAW  xK_b    #! "nuc klomp-cmd playlist books"
        "seek -10"    @@ mCAWS xK_z    #! "nuc klomp-cmd seek -10"
        "seek 10"     @@ mCAWS xK_x    #! "nuc klomp-cmd seek 10"
        "seek -60"    @@ mCAWS xK_a    #! "nuc klomp-cmd seek -60"
        "seek 60"     @@ mCAWS xK_s    #! "nuc klomp-cmd seek 60"
        "stop"        @@ mCAW  xK_c    #! "nuc klomp-cmd stop"

    "Klomp N9"      @@ do
        "pause"       @@ mCW   (xK ' ')#! "n9 -b udo klomp-cmd pause"
        "prev"        @@ mCW   xK_z    #! "n9 -b udo klomp-cmd prev"
        "next"        @@ mCW   xK_x    #! "n9 -b udo klomp-cmd next"
        "books pl"    @@ mCW   xK_b    #! "n9 -b udo klomp-cmd playlist books"
        "seek -10"    @@ mCWS  xK_z    #! "n9 -b udo klomp-cmd seek -10"
        "seek 10"     @@ mCWS  xK_x    #! "n9 -b udo klomp-cmd seek 10"
        "seek -60"    @@ mCWS  xK_a    #! "n9 -b udo klomp-cmd seek -60"
        "seek 60"     @@ mCWS  xK_s    #! "n9 -b udo klomp-cmd seek 60"
        "bar n9/loc"  @@ mCW   xK_c    #! "klomp-bar n9 local"
        "vol 10"      @@ mCW   xK_5    #! "n9 -b udo klomp-cmd volume 10 1"
        "vol 25"      @@ mCW   xK_6    #! "n9 -b udo klomp-cmd volume 25 1"
        "vol 75"      @@ mCW   xK_7    #! "n9 -b udo klomp-cmd volume 75 1"
        "vol 100"     @@ mCW   xK_8    #! "n9 -b udo klomp-cmd volume 100 1"
        "vol -"       @@ mCW   xK_9    #! "n9 -b udo klomp-cmd volume -1 0"
        "vol +"       @@ mCW   xK_0    #! "n9 -b udo klomp-cmd volume +1 0"


windowKeys conf = "Windows" @@ do
    "Current"       @@ do
        "Kill"          @@ [mA xK_F4, mAS xK_c]     # kill
        "Toggle Border" @@ mAS   xK_b    #^ toggleBorder
    "Swap" @@ do
        "To Master"     @@ mAS   xK_Enter## swapMaster
        "Down/Up"   @@  do mAS   xK_j    ## swapDown
                           mAS   xK_k    ## swapUp
        "To Master"     @@ mA    (xK ' ')## swapMaster
    "Move Focus"    @@  do
        "To Master"     @@ mA    xK_Enter## focusMaster
        "Down/Up"   @@  do mA    xK_j    ## focusDown
                           mA    xK_k    ## focusUp
        "Down/Up"   @@  do mA    xK_Tab  ## focusDown
                           mAS   xK_Tab  ## focusUp
    "Sink/Pop Out"  @@  do mA    xK_t    #^ windows . sink
                           mA    xK_u    #^ windows . popout
    "Attach/Detach" @@  do mAW   xK_Enter#  killAllOtherCopies
                           mAWS  xK_Enter## copyToAll
    "Move Floating"     @@ frobWin mC    keysMoveWindow
    "Resize Floating"   @@ frobWin mCA   $ flip keysResizeWindow (0,0)
  where
    popout = flip SS.float $ RationalRect (1/4) (1/4) (1/2) (1/2)
    mag = 20
    frobWin m f = mapM_ (\(k,v) -> m k #^ f v) $ zip arrKeys vs
      where vs = [(-mag, 0), (0, -mag), (mag, 0), (0, mag)]

layoutKeys conf = "Layout" @@ do
    "Restore Default"   @@ mAS   (xK ' ')#  do sinkAll
                                               setLayout $ layoutHook conf
    "Toggle Struts"     @@ mA    xK_f    #> ToggleStruts
    "Left"              @@ mA    xK_a    #> JumpToLayout "left"
    "Top"               @@ mA    xK_s    #> JumpToLayout "top"
    "Full"              @@ mA    xK_d    #> JumpToLayout "full"
    "Grid"              @@ mA    xK_g    #> JumpToLayout "grid"
    "Shrink/Expand" @@  do mA    xK_h    #> Shrink
                           mA    xK_l    #> Expand
    "+/- Master"    @@  do mA    (xK ',')#> IncMasterN 1
                           mA    (xK '.')#> IncMasterN (-1)

workspaceKeys conf = "Workspaces" @@ do
    "Go to"         @@ do
        "<N>"           @@[mA    n       ## greedyView w | (n, w) <- nws]
    "Shift Window"  @@ do
        "<N>"           @@[mAS   n       ## shift w | (n, w) <- nws]
    "Follow Window" @@ do
        "<N>"           @@[mCAS  n       ## follow w   | (n, w) <- nws]
  where
    wss = workspaces conf
    nws = zip [xK_1 .. xK_9] wss
    follow w = greedyView w . shift w

($?) = flip . F.foldr
onWS ws f ss = greedyView (currentTag ss) . f . greedyView ws $ ss

swapWS_W ws ss = onWS ws shiftLast . pushWin . pullWin $ ss
  where
    pushWin = shiftWin ws $? peek ss
    pullWin = shiftWin cw $? (listToMaybe . index . greedyView ws) ss
    cw = currentTag ss

swapWS_D ws = rev . swapWS_W ws . rev
  where rev = onWS ws . modify' $ \(Stack f ls rs) -> Stack f rs ls

shiftLast = modify' $ \s -> case s of
    Stack f ls []     -> Stack f ls []
    Stack f ls (r:rs) -> Stack r ls (rs ++ [f])
