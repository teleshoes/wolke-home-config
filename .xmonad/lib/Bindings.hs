module Bindings where
import Utils (
  readMachineType)

import XMonad
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Actions.FloatKeys (keysMoveWindow, keysResizeWindow)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.WithAll (sinkAll)
import XMonad.Hooks.Focus (toggleLock)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import XMonad.StackSet hiding (focus, workspaces, filter)
import qualified XMonad.StackSet as SS

import Control.Arrow (first)
import Control.Monad (void, (>=>))
import qualified Data.Foldable as F
import Data.List (find, intercalate, isInfixOf, transpose)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Data.Maybe
import System.IO.Error (catchIOError)

import Bindings.Keys
import Bindings.Writer

main = do
  machineType <- readMachineType
  putStr . prettyBindingsIndented $ keyBinds machineType testConfig

tryWriteKeyBindingsCache pretty file = writeKeyBindingsCache pretty file `catchIOError` print

writeKeyBindingsCache pretty file = writeFile file <$> contents =<< readMachineType
  where fmt = if pretty then cols 4 . lines else id
        prettyBindings = if pretty then prettyBindingsFlat else prettyBindingsFlatHex
        binds machineType = keyBinds machineType testConfig
        contents machineType = fmt $ renameWorkspaces $ prettyBindings $ binds machineType

renameWorkspaces = unlines . map rename . lines
  where
    rename line
      | "\"Workspaces\"" `isInfixOf` line
      , Just (old, new) <- findWorkspaceName = intercalate (show new) $ splitOn old line
      | otherwise                            = line
      where
        findWorkspaceName = find ((`isInfixOf` line) . fst) $
            map (first $ show . show) $ zip [1..] workspaceNames

myMouseBindings machineType = M.fromList . bwBindList . mouseBinds machineType
myKeyBindings machineType  = M.fromList . bwBindList . keyBinds machineType

workspaceNames = ["A", "B", "D", "G"] ++ map show [5..9]
testConfig = def{ layoutHook = Layout $ layoutHook def
                , workspaces = workspaceNames }

mouseOverlaps = bwFindOverlap $ mouseBinds Nothing testConfig
keyOverlaps   = bwFindOverlap $ keyBinds   Nothing testConfig

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

mouseBinds machineType conf = "Mouse Bindings" @@ do
    "Move Window"   @@ mW button1 # select >=> mouseMoveWindow
    "Raise Window"  @@ mW button2 # void . select
    "Resize Window" @@ mW button3 # select >=> mouseResizeWindow
  where
    select w = focus w >> windows shiftMaster >> return w

keyBinds machineType conf = "Key Bindings" @@ mapM_ ($ conf)
    [xmoKeys, shortcuts, windowKeys, layoutKeys, workspaceKeys, machineKeys machineType]

restartXmonad msg force = do
  spawn $ "notify-send -t 1000 " ++ msg
  dirs <- io getDirectories
  okRecompile <- recompile dirs force
  let status = if okRecompile then "success" else "failure"
  spawn $ "notify-send -t 1000 xmonad " ++ status
  spawn $ "alarm -s " ++ status
  if okRecompile then restart "xmonad" True else return ()

xmoKeys conf = "XMonad" @@ do
    "Restart Taffybar"  @@ mCA   xK_Home #! "taffybar-restart"
    "Recompile Xmonad"  @@ mCA   xK_End  #  restartXmonad "xmonad recompile" True
    "Restart Xmonad"    @@ mCAS  xK_End  #  restartXmonad "xmonad restart" False
    "Edit Keys"         @@ mCA   xK_Del  #! "term vim ~/.xmonad/lib/Bindings.hs"

machineKeys machineType conf = "Machine" @@ do
  machine "main" $ do
    emptyKeys
  machine "ddr" $ do
    "ddr-player"    @@ m_    xK_Ins  #! "ddr-ui --cycle-player"
  machine "aux" $ do
    emptyKeys
  machine "tv" $ do
    emptyKeys
  machine "bed" $ do
    emptyKeys
  where machine targetMachine binds = if matches targetMachine then targetMachine @@ binds else return ()
        matches targetMachine | isNothing machineType = True --include all for missing machine
        matches targetMachine = fromJust machineType == targetMachine

shortcuts conf = "Shortcuts" @@ do
    "off"               @@ mA    xK_Esc  #! "off g"
    "term"              @@ m_    xK_Think#! "term"
    "term"              @@ m_    xK_AMdia#! "term"
    "term"              @@ m_    xK_Tools#! "term"
    "term"              @@ mA    xK_space#! "term"
    "term auto-cwd"     @@ mA    xK_Tools#! "term -acd"
    "term auto-cwd"     @@ mAW   xK_space#! "term -acd"
    "term ghci"         @@ mC    xK_Tools#! "term ghci -ignore-dot-ghci -ghci-script ~/.ghci"
    "term"              @@ m_    xK_Rfrsh#! "term"
    "term"              @@ mA    xK_F2   #! "term"
    "term auto-cwd"     @@ mA    xK_Think#! "term -acd"
    "term top"          @@ mW    xK_Up   #! "term-window --top"
    "term bottom"       @@ mW    xK_Down #! "term-window --bottom"
    "term left"         @@ mW    xK_Left #! "term-window --left"
    "term right"        @@ mW    xK_Right#! "term-window --right"

    "ghci"              @@ mC    xK_Think#! "term ghci -ignore-dot-ghci -ghci-script ~/.ghci"
    "ghci"              @@ mA    xK_F3   #! "term ghci -ignore-dot-ghci -ghci-script ~/.ghci"
    "bgset --next"      @@ mW    xK_w    #! "bgset --next"
    "bgset --prev"      @@ mWS   xK_w    #! "bgset --prev"
    "screenshot"        @@ m_    xK_Print#! "scrot-bag --flash ~/Pictures/Screenshots"
    "screenshot-quick"  @@ mS    xK_Print#! "scrot-bag ~/Pictures/Screenshots"
    "Invert Colors"     @@ mW    xK_n    #! "xcalib -i -a"
    "Screen Off Idle"   @@ mW    xK_s    #! "sleep 0.5; screenOff"
    "Screen On/Off"     @@ mCW   xK_s    #! "screenpwr"
    "touchclick toggle" @@ mC    xK_Menu #! "touchClick toggle"
    "htop"              @@ mCA   xK_space#! "term htop"
    "ramfix-inodes"     @@ mCA   xK_m    #! "ramfix-inodes; notify-send -t 1000 'RAN ramfix-inodes'"

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
                           mW    xK_F3   #! "pulse-vol +6 150"
                           mC    xK_F3   #! "pulse-vol +6 300"
        down        @@  do m_    xK_VolDn#! "pulse-vol -6 100"
                           mA    xK_VolDn#! "pulse-vol -6 150"
                           mW    xK_F2   #! "pulse-vol -6 150"
                           mC    xK_VolDn#! "pulse-vol -6 300"

        "Toggle mute"@@ do m_    xK_Mute #! "pulse-vol speaker toggle"
                           mW    xK_F1   #! "pulse-vol speaker toggle"
        "Force mute"    @@ mA    xK_Mute #! "pulse-vol 0; pulse-vol speaker mute"

        "Mute Mic"   @@ do m_    xK_Mic  #! "pulse-vol microphone toggle"
                           m_    xK_Power#! "pulse-vol microphone toggle"
                           mW    xK_F4   #! "pulse-vol microphone toggle"

        "spkr switch"   @@ mW    xK_VolUp#! "speaker toggle; klomp-cmd restart"
        "AVR volup"     @@ mC    xK_equal#! "avr volup"
        "AVR voldn"     @@ mC    xK_minus#! "avr voldown"
        "AVR mute"      @@ mC    xK_Mute #! "avr --mute-toggle"
        "igcmd volup"   @@ mCS   xK_equal#! "ipmagic pi igcmd av-vol_up"
        "igcmd voldn"   @@ mCS   xK_minus#! "ipmagic pi igcmd av-vol_down"
        "tv vol up"     @@ mCA   xK_VolUp#! "ipmagic tv -s pulse-vol +6 100"
        "tv vol down"   @@ mCA   xK_VolDn#! "ipmagic tv -s pulse-vol -6 100"
        "tv spkr cyc"   @@ mCAW  xK_VolUp#! "ipmagic tv -s speaker --cycle; ipmagic tv -s klomp-cmd restart"

    "iguana IR"     @@ do
        "AC / fan"      @@ mC    xK_F1   #! "ac"

    "CEC power"     @@ do
        "TV pwr toggle" @@ mC    xK_F2   #! "ipmagic tv cec-cmd --tv togglestandby"
        "TV pwr toggle" @@ mCA   xK_Ins  #! "ipmagic tv cec-cmd --tv togglestandby"

    "CPU"           @@ do
        "100%"          @@ mCWS  xK_F1   #! "sudo cpu-set 0% 100%"
        "66%"           @@ mCWS  xK_F2   #! "sudo cpu-set 0% 66%"
        "33%"           @@ mCWS  xK_F3   #! "sudo cpu-set 0% 33%"
        "0%"            @@ mCWS  xK_F4   #! "sudo cpu-set 0% 0%"

    "Fan"           @@ do
        "auto"          @@ mW    xK_F5   #! "screen-daemon fan --start && sudo fan default"
        "fast"          @@ mW    xK_F6   #! "screen-daemon fan --stop  && sudo fan fast"
        "medium"        @@ mW    xK_F7   #! "screen-daemon fan --stop  && sudo fan medium"
        "slow"          @@ mW    xK_F8   #! "screen-daemon fan --stop  && sudo fan slow"

    "feh"           @@ do
        "left"          @@ m_    xK_ScrLk#! "feh-left"
        "right"         @@ m_    xK_Pause#! "feh-right"
        "left"          @@ mA    xK_F11  #! "feh-left"
        "right"         @@ mA    xK_F12  #! "feh-right"
        "left"          @@ mC    xK_Left #! "feh-left"
        "right"         @@ mC    xK_Right#! "feh-right"

    "games"         @@ do
        "kingdom-save"  @@ mAW   xK_F7   #! "kingdom-save"
        "kingdom-pause" @@ mAW   xK_F8   #! "kingdom-pause"

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
        "Chrome"        @@ mCAS  xK_c    #! "google-chrome --simulate-outdated-no-au='Tue, 31 Dec 2099 23:59:59 GMT'"
        "steam"         @@ mCA   xK_t    #! "term steam"
        "jitsi-meet"    @@ mCA   xK_j    #! "jitsi-meet"
        "zoom"          @@ mCA   xK_z    #! "zoom-run"
        "hangout"       @@ mCA   xK_h    #! "hangout"
        "hex-a-hop"     @@ mCA   xK_x    #! "hex-a-hop"
        "tor"           @@ mCA   xK_v    #! "tor"
        "sabnzbd"       @@ mCA   xK_b    #! "sabnzbd"
        "Pidgin"        @@ mCA   xK_p    #! "pidgin"
        "Eclipse"       @@ mCA   xK_e    #! "eclipse"
        "Rhythmbox"     @@ mCA   xK_r    #! "rhythmbox"
        "sheetmusic"    @@ mCA   xK_s    #! "sheetmusic -w -d -p --player-ipmagic=tv"
        "sheetmusic lf" @@ mCAS  xK_s    #! "sheetmusic -w -d -p --player-ipmagic=tv --no-last-file little_fugue"
        "stepmania"     @@ mCA   xK_d    #! "stepmania-launch"
        "jstest-tone"   @@ mCA   xK_i    #! "jstest-tone"

    "tv"           @@ do
        "vnc"           @@ mC    xK_F9   #! "ipmagic tv -vnc"
        "vnc"           @@ mCS   xK_F9   #! "ipmagic tv -vnc --x2vnc"
        "bgset --next"  @@ mCAW  xK_w    #! "ipmagic tv bgset --next"
        "bgset --prev"  @@ mCAWS xK_w    #! "ipmagic tv bgset --prev"

    "tasmota"         @@ do
        "ts lamp1"      @@ mC    xK_PgDn #! "tasmota lamp1"
        "ts liv"        @@ mC    xK_PgUp #! "tasmota liv"
        "ts l"          @@ mCS   xK_PgDn #! "tasmota l"

    "sx"            @@ do
        "vnc"           @@ mC    xK_F11  #! "ipmagic sx -vnc"
        "vnc landscape" @@ mCS   xK_F11  #! "ipmagic sx -vnc --landscape"
        "lock"          @@ mC    xK_F12  #! "ipmagic sx lock"

    "Klomp"         @@ do
        "pause"       @@ mW    xK_space#! "klomp-cmd pause"
        "prev"        @@ mW    xK_z    #! "klomp-cmd prev"
        "next"        @@ mW    xK_x    #! "klomp-cmd next"
        "books pl"    @@ mW    xK_b    #! "klomp-cmd playlist books"
        "seek -10"    @@ mWS   xK_z    #! "klomp-cmd seek -10"
        "seek +10"    @@ mWS   xK_x    #! "klomp-cmd seek +10"
        "seek -60"    @@ mWS   xK_a    #! "klomp-cmd seek -60"
        "seek +60"    @@ mWS   xK_s    #! "klomp-cmd seek +60"
        "save"        @@ mW    xK_v    #! "save-klomp"
        "bigtext"     @@ mWS   xK_d    #! "klomp-bigtext"
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
        "pause"       @@ mCAW  xK_space#! "ipmagic tv klomp-cmd pause"
        "prev"        @@ mCAW  xK_z    #! "ipmagic tv klomp-cmd prev"
        "next"        @@ mCAW  xK_x    #! "ipmagic tv klomp-cmd next"
        "books pl"    @@ mCAW  xK_b    #! "ipmagic tv klomp-cmd playlist books"
        "seek -10"    @@ mCAWS xK_z    #! "ipmagic tv klomp-cmd seek -10"
        "seek +10"    @@ mCAWS xK_x    #! "ipmagic tv klomp-cmd seek +10"
        "seek -60"    @@ mCAWS xK_a    #! "ipmagic tv klomp-cmd seek -60"
        "seek +60"    @@ mCAWS xK_s    #! "ipmagic tv klomp-cmd seek +60"
        "stop"        @@ mCAW  xK_c    #! "ipmagic tv klomp-cmd stop"

    "Klomp sx"      @@ do
        "pause"       @@ mCW   xK_space#! "ipmagic sx klomp-coolreader --play-pause"
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
    "Lock Focus"        @@ mAS   xK_l    # toggleLock
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
    "Move Floating" @@  do
                           mCW   xK_Up   #^ moveWindow (0, -winPx)
                           mCW   xK_Down #^ moveWindow (0, winPx)
                           mCW   xK_Left #^ moveWindow (-winPx, 0)
                           mCW   xK_Right#^ moveWindow (winPx, 0)
    "Resize Floating" @@  do
                           mCA   xK_Up   #^ resizeWindowFromTopLeft (0, -winPx)
                           mCA   xK_Down #^ resizeWindowFromTopLeft (0, winPx)
                           mCA   xK_Left #^ resizeWindowFromTopLeft (-winPx, 0)
                           mCA   xK_Right#^ resizeWindowFromTopLeft (winPx, 0)
  where
    popout = flip SS.float $ RationalRect (1/4) (1/4) (1/2) (1/2)
    moveWindow = keysMoveWindow . intToDim
    resizeWindowFromTopLeft = flip keysResizeWindow (0,0) . intToDim
    intToDim (x,y) = (fromIntegral x, fromIntegral y)
    winPx = 22

layoutKeys conf = "Layout" @@ do
    "Restore Default"   @@ mAS   xK_space#  do sinkAll
                                               setLayout $ layoutHook conf
    "Toggle Struts"     @@ mA    xK_f    #> ToggleStruts
    "Left"              @@ mA    xK_a    #> JumpToLayout "left"
    "Top"               @@ mA    xK_s    #> JumpToLayout "top"
    "Full"              @@ mA    xK_d    #> JumpToLayout "full"
    "Grid"              @@ mA    xK_g    #> JumpToLayout "grid"
    "Cols"              @@ mA    xK_c    #> JumpToLayout "cols"
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
