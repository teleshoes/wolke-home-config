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
testConfig = def{ layoutHook = Layout $ layoutHook def
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
    "off"               @@ mA    xK_Esc  #! "off g"
    "term"              @@ m_    xK_Think#! "term"
    "term"              @@ m_    xK_AMdia#! "term"
    "term"              @@ m_    xK_Tools#! "term"
    "term"              @@ mA    xK_space#! "term"
    "term auto-cwd"     @@ mA    xK_Tools#! "term -acd"
    "term auto-cwd"     @@ mAW   xK_space#! "term -acd"
    "term ghci"         @@ mC    xK_Tools#! "term ghci"
    "term"              @@ m_    xK_Rfrsh#! "term"
    "term"              @@ mA    xK_F2   #! "term"
    "term auto-cwd"     @@ mA    xK_Think#! "term -acd"
    "term top"          @@ mW    xK_Up   #! "term-window --top"
    "term bottom"       @@ mW    xK_Down #! "term-window --bottom"
    "term left"         @@ mW    xK_Left #! "term-window --left"
    "term right"        @@ mW    xK_Right#! "term-window --right"

    "ghci"              @@ mC    xK_Think#! "term ghci"
    "ghci"              @@ mA    xK_F3   #! "term ghci"
    "bgset --next"      @@ mW    xK_w    #! "bgset --next"
    "bgset --prev"      @@ mWS   xK_w    #! "bgset --prev"
    "screenshot"        @@ m_    xK_Print#! "scrot-bag ~/Pictures/Screenshots"
    "Invert Colors"     @@ mW    xK_n    #! "xcalib -i -a"
    "Screen Off Idle"   @@ mW    xK_s    #! "sleep 0.5; screenOff"
    "Screen On/Off"     @@ mCW   xK_s    #! "screenpwr"
    "thinklight"        @@ mC    xK_PgUp #! "led thinklight"
    "touchclick toggle" @@ mC    xK_Menu #! "touchClick toggle"
    "htop"              @@ mCA   (xK ' ')#! "term htop"

    "Network"       @@ do
        "wauto"         @@ mW    xK_grave#! "sudo wauto"
        "wifi-fix"      @@ mWS   xK_grave#! "term wifi-fix"
        "off"           @@ mCW   xK_grave#! "sudo wconnect -d; " ++
                                            "sudo tether off; " ++
                                            "sudo wired off"

    "Resolution"    @@ do
        "alt1"          @@ mCW   xK_F1   #! "resconfig --set-res 1"
        "alt2"          @@ mCW   xK_F2   #! "resconfig --set-res 2"
        "alt3"          @@ mCW   xK_F3   #! "resconfig --set-res 3"
        "alt4"          @@ mCW   xK_F4   #! "resconfig --set-res 4"

    "Brightness"    @@ do
        "Up"            @@ mC    xK_Home #! "brightness up"
        "Down"          @@ mC    xK_End  #! "brightness down"
        "Up"            @@ m_    xK_BriUp#! "brightness up"
        "Down"          @@ m_    xK_BriDn#! "brightness down"

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
        "Mute Mic"      @@ m_    xK_Power#! "pulse-vol microphone toggle"
        "spkr switch"   @@ mW    xK_VolUp#! "speaker toggle; klomp-cmd restart"
        "tv cec mute"   @@ mC    xK_Mute #! "ipmagic tv cec-cmd --av mute"
        "tv cec start"  @@ mCS   xK_Mute #! "ipmagic tv cec-cmd start"
        "tv cec volup"  @@ mC    xK_equal#! "ipmagic tv cec-cmd --av vol +1"
        "tv cec voldn"  @@ mC    xK_minus#! "ipmagic tv cec-cmd --av vol -1"
        "igcmd volup"   @@ mCS   xK_equal#! "ipmagic pi igcmd av-vol_up"
        "igcmd voldn"   @@ mCS   xK_minus#! "ipmagic pi igcmd av-vol_down"
        "tv vol up"     @@ mCA   xK_VolUp#! "ipmagic tv pulse-vol +6 100"
        "tv vol down"   @@ mCA   xK_VolDn#! "ipmagic tv pulse-vol -6 100"
        "tv spkr cyc"   @@ mCAW  xK_VolUp#! "ipmagic tv speaker --cycle; ipmagic tv klomp-cmd restart"

    "iguana IR"     @@ do
        "A/C toggle"    @@ mC    xK_F1   #! "term ac toggle"
        "A/C power"     @@ mCS   xK_F1   #! "ac ac-power"
        "A/C mode"      @@ mC    xK_F3   #! "ac ac-mode"

    "CEC power"     @@ do
        "TV pwr toggle" @@ mC    xK_F2   #! "ipmagic tv cec-cmd --tv togglestandby"
        "TV pwr toggle" @@ mCA   xK_Ins  #! "ipmagic tv cec-cmd --tv togglestandby"

    "CPU"           @@ do
        "100%"          @@ mW    xK_F1   #! "sudo cpu-set 100%"
        "66%"           @@ mW    xK_F2   #! "sudo cpu-set 66%"
        "33%"           @@ mW    xK_F3   #! "sudo cpu-set 33%"
        "0%"            @@ mW    xK_F4   #! "sudo cpu-set 0%"

    "Fan"           @@ do
        "auto"          @@ mW    xK_F5   #! "screen-daemon fan --start"
        "fast"          @@ mW    xK_F6   #! "screen-daemon fan --stop && sudo fan fast"
        "medium"        @@ mW    xK_F7   #! "screen-daemon fan --stop && sudo fan medium"
        "slow"          @@ mW    xK_F8   #! "screen-daemon fan --stop && sudo fan slow"

    "feh"           @@ do
        "left"          @@ m_    xK_ScrLk#! "feh-left"
        "right"         @@ m_    xK_Pause#! "feh-right"
        "left"          @@ mA    xK_F11  #! "feh-left"
        "right"         @@ mA    xK_F12  #! "feh-right"
        "left"          @@ mC    xK_Left #! "feh-left"
        "right"         @@ mC    xK_Right#! "feh-right"

    "games"         @@ do
        "kingdom-save"  @@ mAW   xK_F7   #! "kingdom-save"

    "alarms"        @@ do
        "coffee!"       @@ mW    xK_c    #! "fcron-job-toggle co toggle"
        "tea!"          @@ mW    xK_t    #! "fcron-job-toggle te toggle"

    "Rotate Deasil/Widdershins" @@ do
                           mCA   xK_Fwd  #! "rotate deasil"
                           mCA   xK_Back #! "rotate widdershins"
                           mCA   xK_PgDn #! "rotate deasil"
                           mCA   xK_PgUp #! "rotate widdershins"

    "Applications"  @@ do
        "kodi"          @@ mCA   xK_k    #! "kodi"
        "Firefox"       @@ mCA   xK_f    #! "firefox"
        "Firefox 24"    @@ mCAS  xK_f    #! "ff24"
        "Chrome Incog"  @@ mCA   xK_c    #! "chromium --incognito"
        "Chrome"        @@ mCAS  xK_c    #! "chromium"
        "steam"         @@ mCA   xK_t    #! "term steam"
        "jitsi-meet"    @@ mCA   xK_j    #! "jitsi-meet"
        "zoom"          @@ mCA   xK_z    #! "zoom-run"
        "hangout"       @@ mCA   xK_h    #! "hangout"
        "hex-a-hop"     @@ mCA   xK_x    #! "hex-a-hop"
        "tor"           @@ mCA   xK_v    #! "tor"
        "sabnzbd"       @@ mCA   xK_b    #! "sabnzbd"
        "Pidgin"        @@ mCA   xK_p    #! "pidgin"
        "Eclipse"       @@ mCA   xK_e    #! "eclipse"
        "FBreader"      @@ mCA   xK_r    #! "fbreader"
        "sheetmusic"    @@ mCA   xK_s    #! "sheetmusic"
        "stepmania -i"  @@ mCA   xK_i    #! "stepmania -i"
        "smbc"          @@ mCA   xK_m    #! "smbc"

    "tv"           @@ do
        "vnc"           @@ mC    xK_F9   #! "ipmagic tv -vnc"
        "vnc"           @@ mCS   xK_F9   #! "ipmagic tv -vnc --x2vnc"
        "bgset --next"  @@ mCAW  xK_w    #! "ipmagic tv bgset --next"
        "bgset --prev"  @@ mCAWS xK_w    #! "ipmagic tv bgset --prev"

    "wemo"         @@ do
        "wemo-lights"   @@ mC    xK_PgDn #! "wemo-switch wemo1 --toggle"

    "sx"            @@ do
        "vnc"           @@ mC    xK_F11  #! "vnc-sx"
        "lock"          @@ mC    xK_F12  #! "ipmagic sx lock"

    "Klomp"         @@ do
        "pause"       @@ mW    (xK ' ')#! "klomp-cmd pause"
        "prev"        @@ mW    xK_z    #! "klomp-cmd prev"
        "next"        @@ mW    xK_x    #! "klomp-cmd next"
        "books pl"    @@ mW    xK_b    #! "klomp-cmd playlist books"
        "seek -10"    @@ mWS   xK_z    #! "klomp-cmd seek -10"
        "seek +10"    @@ mWS   xK_x    #! "klomp-cmd seek +10"
        "seek -60"    @@ mWS   xK_a    #! "klomp-cmd seek -60"
        "seek +60"    @@ mWS   xK_s    #! "klomp-cmd seek +60"
        "save"        @@ mW    xK_v    #! "save-klomp"
        "bigtext"     @@ mW    xK_d    #! "klomp-bigtext"
                                          ++ " --width=$(( $(res -w --percent=98) ))"
                                          ++ " --height=$(( $(res -h --percent=98) - $(taffybar-height) ))"
                                          ++ " --window"
                                          ++ " --toggle"

    "Klomp media keys"    @@ do
        "play/pause"  @@ m_    xK_APlay#! "klomp-cmd pause"
        "pause/play"  @@ m_    xK_APaus#! "klomp-cmd pause"
        "next"        @@ m_    xK_ANext#! "audio-key next"
        "prev"        @@ m_    xK_APrev#! "audio-key prev"

    "Klomp tv"     @@ do
        "pause"       @@ mCAW  (xK ' ')#! "ipmagic tv klomp-cmd pause"
        "prev"        @@ mCAW  xK_z    #! "ipmagic tv klomp-cmd prev"
        "next"        @@ mCAW  xK_x    #! "ipmagic tv klomp-cmd next"
        "books pl"    @@ mCAW  xK_b    #! "ipmagic tv klomp-cmd playlist books"
        "seek -10"    @@ mCAWS xK_z    #! "ipmagic tv klomp-cmd seek -10"
        "seek +10"    @@ mCAWS xK_x    #! "ipmagic tv klomp-cmd seek +10"
        "seek -60"    @@ mCAWS xK_a    #! "ipmagic tv klomp-cmd seek -60"
        "seek +60"    @@ mCAWS xK_s    #! "ipmagic tv klomp-cmd seek +60"
        "stop"        @@ mCAW  xK_c    #! "ipmagic tv klomp-cmd stop"

    "Klomp sx"      @@ do
        "pause"       @@ mCW   (xK ' ')#! "ipmagic sx klomp-cmd pause"
        "prev"        @@ mCW   xK_z    #! "ipmagic sx klomp-cmd prev"
        "next"        @@ mCW   xK_x    #! "ipmagic sx klomp-cmd next"
        "books pl"    @@ mCW   xK_b    #! "ipmagic sx klomp-cmd playlist books"
        "seek -10"    @@ mCWS  xK_z    #! "ipmagic sx klomp-cmd seek -10"
        "seek +10"    @@ mCWS  xK_x    #! "ipmagic sx klomp-cmd seek +10"
        "seek -60"    @@ mCWS  xK_a    #! "ipmagic sx klomp-cmd seek -60"
        "seek +60"    @@ mCWS  xK_s    #! "ipmagic sx klomp-cmd seek +60"
        "bar sx/loc"  @@ mCW   xK_c    #! "klomp-bar --toggle sx"


windowKeys conf = "Windows" @@ do
    "Current"       @@ do
        "Kill"          @@ [mA xK_F4, mAS xK_c]     # kill
        "Toggle Border" @@ mAS   xK_b    #^ toggleBorder
    "Swap" @@ do
        "To Master"     @@ mAS   xK_Enter## swapMaster
        "Down/Up"   @@  do mAS   xK_j    ## swapDown
                           mAS   xK_k    ## swapUp
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
    "Move Floating"     @@ frobWin mCW   keysMoveWindow
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
