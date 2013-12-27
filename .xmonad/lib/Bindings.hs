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

import Control.Applicative
import qualified Data.Foldable as F
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe

import Bindings.Keys
import Bindings.Writer

workspaceNames = map show [1..9]
testConfig = defaultConfig{ layoutHook = Layout $ layoutHook defaultConfig
                          , workspaces = workspaceNames }

myMouseBindings = M.fromList . bwBindList . mouseBinds
myKeyBindings   = M.fromList . bwBindList . keyBinds

mouseOverlaps = bwFindOverlap $ mouseBinds testConfig
keyOverlaps   = bwFindOverlap $ keyBinds   testConfig

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
    [xmoKeys, shortcuts, musicKeys, windowKeys, layoutKeys, workspaceKeys]

xmoKeys conf = "XMonad" @@ do
    "Restart"   @@ mCA xK_Home #! "taffybar-restart"
    "Recompile" @@ mCA xK_End  #! "xmonad-restart"
    "Bindings"  @@ mCA xK_Ins  #! "term vim ~/.xmonad/lib/Bindings.hs"

shortcuts conf = "Shortcuts" @@ do
    "off"               @@ [m_ xK_Power, mA xK_Esc, mCA xK_Del] #! "off g"
    "term auto-cwd"     @@ m_    xK_Think#! "term -acd"
    "term"              @@ mA    xK_F2   #! "term"
    "ghci"              @@ mA    xK_Think#! "term ghci"
    "Screen Shot"       @@ m_    xK_Print#! "scrot-bag"
    "Invert Colors"     @@ mW    xK_n    #! "xcalib -i -a"
    "Screen On/Off" @@  do mCA   xK_Up   #! "sudo screenpwr on"
                           mCA   xK_Down #! "sudo screenpwr off"
    "Brightness"    @@ do
        "Up"            @@ mC    xK_Home #! "brightness up"
        "Down"          @@ mC    xK_End  #! "brightness down"
        "{system up}"   @@ m_    xK_BriUp#! "brightness system"
        "{system down}" @@ m_    xK_BriDn#! "brightness system"
    "Sound"         @@ do
        let [up,down] = map (++ ",100/150/300") ["+","-"]
        up          @@  do m_    xK_VolUp#! "pulse-vol +5 100"
                           mA    xK_VolUp#! "pulse-vol +5 150"
                           mC    xK_VolUp#! "pulse-vol +5 300"
        down        @@  do m_    xK_VolDn#! "pulse-vol -5 100"
                           mA    xK_VolDn#! "pulse-vol -5 150"
                           mC    xK_VolDn#! "pulse-vol -5 300"
        "Mute Sound"    @@ m_    xK_Mute #! "pulse-vol speaker toggle"
        "Mute Mic"      @@ mA    xK_Mute #! "pulse-vol microphone toggle"
    "CPU"           @@ do
        "On Demand"     @@ mC    xK_F1   #! "sudo cpu-set ondemand"
        "Conservative"  @@ mC    xK_F2   #! "sudo cpu-set conservative"
        "Powersave"     @@ mC    xK_F3   #! "sudo cpu-set powersave"
        "Performance"   @@ mC    xK_F4   #! "sudo cpu-set performance"
    "Rotate Deasil/Widdershins" @@ do
                           mCA   xK_Fwd  #! "rotate deasil"
                           mCA   xK_Back #! "rotate widdershins"
    "Applications"  @@ do
        "Firefox"       @@ mCA   xK_f    #! "firefox"
        "Chrome"        @@ mCA   xK_c    #! "chromium --incognito"
        "Pidgin"        @@ mCA   xK_p    #! "pidgin"
        "Transmission"  @@ mCA   xK_t    #! "transmission-gtk"
        "FBReader"      @@ mCA   xK_b    #! "fbreader"

musicKeys conf = "Music" @@ do
    "klomp"         @@     mW    xK_a    #! "term klomp"
    "edit playlist" @@     mW    xK_e    #! "term vim .klomplist"
    "play/pause"    @@     mW   (xK ' ') #! "klomp-cmd pause"
    "play/pause"    @@     m_    xK_Pause#! "klomp-cmd pause"
    "prev"          @@     mW   (xK ',') #! "klomp-cmd prev"
    "next"          @@     mW   (xK '.') #! "klomp-cmd next"
    let [forward,back] = map (\sign -> "seek "++ sign ++ "3/10/60/600") ["+","-"]
    forward     @@  do mW    xK_Fwd #! "klomp-cmd seek +3"
                       mW    xK_Right#! "klomp-cmd seek +10"
                       mW    xK_Up   #! "klomp-cmd seek +60"
                       mW    xK_PgUp #! "klomp-cmd seek +600"
    back        @@  do mW    xK_Back #! "klomp-cmd seek -3"
                       mW    xK_Left #! "klomp-cmd seek -10"
                       mW    xK_Down #! "klomp-cmd seek -60"
                       mW    xK_PgDn #! "klomp-cmd seek -600"

windowKeys conf = "Windows" @@ do
    "Current"       @@ do
        "Kill"          @@ [mA xK_F4, mAS xK_c]     # kill
        "Refresh"       @@ mA    xK_F5   #  refresh
        "Toggle Border" @@ mAS   xK_b    #^ toggleBorder
    "Swap" @@ do
        "To Master"     @@ mA   (xK ' ') ## swapMaster
        "Down/Up"   @@  do mAS   xK_j    ## swapDown
                           mAS   xK_k    ## swapUp
    "Move Focus"    @@ do
        "To Master"     @@ mAS  (xK ' ') ## focusMaster
        "Down/Up"   @@  do mA    xK_j    ## focusDown
                           mA    xK_k    ## focusUp
        "Down/Up"   @@  do mA    xK_Tab  ## focusDown
                           mAS   xK_Tab  ## focusUp
    "Sink/Pop Out"  @@  do mW    xK_Enter#^ windows . sink
                           mWS   xK_Enter#^ windows . popout
    "Attach/Detach" @@  do mAW   xK_Enter#  killAllOtherCopies
                           mAWS  xK_Enter## copyToAll
    "Move Floating"     @@ frobWin mAW keysMoveWindow
    "Resize Floating"   @@ frobWin mWS $ flip keysResizeWindow (0,0)
  where
    popout = flip SS.float $ RationalRect (1/4) (1/4) (1/2) (1/2)
    mag = 10
    frobWin m f = mapM_ (\(k,v) -> m k #^ f v) $ zip arrKeys vs
      where vs = [(-mag, 0), (0, -mag), (mag, 0), (0, mag)]

layoutKeys conf = "Layout" @@ do
    "Restore Default"   @@ mC   (xK ' ') #  do sinkAll
                                               setLayout $ layoutHook conf
    "Toggle Struts"     @@ mA    xK_d    #> ToggleStruts
    "Left"              @@ mA    xK_a    #> JumpToLayout "left"
    "Top"               @@ mA    xK_w    #> JumpToLayout "top"
    "Full"              @@ mA    xK_s    #> JumpToLayout "full"
    "Shrink/Expand" @@  do mA    xK_h    #> Shrink
                           mA    xK_l    #> Expand
    "+/- Master"    @@  do mA   (xK ',') #> IncMasterN 1
                           mA   (xK '.') #> IncMasterN (-1)

workspaceKeys conf = "Workspaces" @@ do
    "Go to"         @@ do
        "Next/Prev" @@  do mCA   xK_Right## viewNext
                           mCA   xK_Left ## viewPrev
        "<N>"           @@[mA    n       ## greedyView w | (n, w) <- nws]
    "Shift Window"  @@ do
        "Next/Prev" @@  do mAS   xK_Right## shiftNext
                           mAS   xK_Left ## shiftPrev
        "<N>"           @@[mAS   n       ## shift w | (n, w) <- nws]
    "Follow Window" @@ do
        "Next/Prev" @@  do mCAS  xK_Right## viewNext . shiftNext
                           mCAS  xK_Left ## viewPrev . shiftPrev
        "<N>"           @@[mCAS  n       ## follow w   | (n, w) <- nws]
    "Screens"       @@ do
        "View <N>"      @@[mAW   n       #  screen s view   | (n, s) <- nss]
        "Shift <N>"     @@[mAWS  n       #  screen s shift  | (n, s) <- nss]
        "Follow <N>"    @@[mCAWS n       #  screen s follow | (n, s) <- nss]
    "Swap Next W/D" @@  do mCA   xK_Tab  ## flip swapWS_W <*> next . currentTag
                           mCAS  xK_Tab  ## flip swapWS_D <*> next . currentTag
  where
    wss = workspaces conf
    nws = zip [xK_1 .. xK_9] wss
    nss = zip [xK_F1 .. xK_F12] [0..]

    adj xs = M.fromList . zip xs $ tail (cycle xs)
    [next, prev] = map (\ls -> (adj ls !)) [wss, reverse wss]
    onAdjs a = map (\m s -> a (m $ currentTag s) s) [next, prev]
    [viewNext , viewPrev]  = onAdjs greedyView
    [shiftNext, shiftPrev] = onAdjs shift
    follow w = greedyView w . shift w
    screen s a = screenWorkspace s >>= flip whenJust (windows . a)

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

