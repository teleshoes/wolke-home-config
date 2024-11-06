{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
import Bindings (
  workspaceNames, myKeyBindings, myMouseBindings, keyOverlaps, mouseOverlaps,
  tryWriteKeyBindingsCache)
import StaticAssert (staticAssert)
import Utils (
  readMachineType, relToHomeDir)

import XMonad (
  xmonad,
  (<+>), (=?), (-->),
  XConfig(..), def, mod1Mask,
  Query,
  Tall(..), Mirror(..), Full(..),
  Window(..), X(..), Event(..),
  ask, title, className, doF, doFloat, doIgnore, doShift,
  getAtom, runQuery, changeProperty32, propModeReplace,
  sendMessage, io, spawn, killWindow, liftX, refresh, windows)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.Focus (
  FocusHook(..), focused, keepFocus, manageFocus, switchFocus)
import XMonad.Hooks.ManageHelpers ((-?>), doFullFloat, isFullscreen, composeOne)
import XMonad.Hooks.ManageDocks (
  SetStruts(..),
  avoidStruts, docks, manageDocks)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(..))
import XMonad.Layout.Renamed (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.StackSet (
  RationalRect(..), float, sink, view, focusDown)
import XMonad.Util.Types (Direction2D(U,D,L,R))

import System.Taffybar.Support.PagerHints (pagerHints)

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Writer (execWriter, tell)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Monoid (All(..))
import Data.Function ((&))

staticAssert (null mouseOverlaps && null keyOverlaps) . execWriter $ do
    tell "Error: Overlap in bindings\n"
    let pretty = tell . unlines . map ((replicate 8 ' ' ++) . show . map fst)
    pretty mouseOverlaps
    pretty keyOverlaps

ffExec = "iceweasel"
ffName = "Iceweasel"

main = do
  machineType <- readMachineType
  xmonad . ewmh . pagerHints . docks $ def
    { focusFollowsMouse  = False
    , modMask            = mod1Mask
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#ff0000"
    , borderWidth        = 3

    , handleEventHook    = myEventHook
    , startupHook        = myStartupHook
    , layoutHook         = myLayoutHook & avoidStruts
    , manageHook         = myManageHook <+> manageDocks <+> manageFocus myNewFocusHook

    , workspaces         = workspaceNames
    , keys               = myKeyBindings machineType
    , mouseBindings      = myMouseBindings machineType
    }

myNewFocusHook :: FocusHook
myNewFocusHook = composeOne
  [ focused (className =? "xmonad-keep-focus") -?> keepFocus
  , return True -?> switchFocus --default
  ]

myStartupHook = do
  spawn "find $HOME/.xmonad/ -regex '.*\\.\\(hi\\|o\\)' -delete"
  io $ tryWriteKeyBindingsCache False =<< relToHomeDir ".cache/xmonad-bindings"
  io $ tryWriteKeyBindingsCache True =<< relToHomeDir ".cache/xmonad-bindings-pretty"

myLayoutHook = smartBorders
             $   ( named "left"
                   $ onWorkspace "B" (Tall 1 incr ratioWide)
                   $ onWorkspace "9" (Tall 1 incr ratioDDR)
                   $ (Tall 1 incr ratio)
                 )
             ||| ( named "top"
                   $ (Mirror $ Tall 1 incr ratio)
                 )
             ||| ( named "full"
                   $ (Full)
                 )
             ||| ( named "grid"
                   $ (Grid)
                 )
             ||| ( named "cols"
                   $ (Mirror $ Tall 10 incr ratio)
                 )
  where incr = 3/100
        ratio = 55/100
        ratioWide = 80/100
        ratioDDR = 82/100

(~?) :: Query String -> String -> Query Bool
q ~? s = fmap (isInfixOf s) q
(^?) :: Query String -> String -> Query Bool
q ^? s = fmap (isPrefixOf s) q
($?) :: Query String -> String -> Query Bool
q $? s = fmap (isSuffixOf s) q

myManageHook = execWriter $ do
  let a ~~> b = tell (a --> b)
  isFullscreen ~~> doFullFloat
  className =? "Qtbigtext.py"          ~~> doUnfloat --only unfloats non-fullscreen
  title     =? "Find/Replace "         ~~> doFloat
  className =? "Eclipse"               ~~> (doShift "A" <+> doUnfloat)
  title     =? "GWT Development Mode"  ~~> doShift "G"
  className =? "Pidgin"                ~~> doShift "B"
  title     =? "email-gui.py"          ~~> doShift "8"
  title     =? "qtemail-daemon"        ~~> doShift "9"
  title     =? "charge-limit-daemon"   ~~> doShift "9"
  className =? "feh"                   ~~> doFloat
  title     =? "Off"                   ~~> doFloat
  title     =? "Transmission"          ~~> doShift "9"
  className =? "Transmission-gtk"      ~~> doUnfloat
  title     =? "Torrent Options"       ~~> doShiftView "9"
  title     =? ("Close " ++ ffName)    ~~> restartFF
  title     =? "StepMania"             ~~> doFloat
  title     =? "npviewer.bin"          ~~> doFloat -- flash
  title     =? "plugin-container"      ~~> doFloat -- flash
  title     =? "xfce4-notifyd"         ~~> doIgnore
  title     ^? "term-window"           ~~> doFloat
  title     ^? "xmonad-float"          ~~> doFloat
  title     ^? "xmonad-float-unfocus"  ~~> (doFloat <+> doF focusDown)

restartFF = do
  w <- ask
  let delay = 1
  let msg = "'restarting " ++ ffExec ++ " in " ++ show delay ++ "s'"
  liftX $ do
    spawn $ "killall -9 " ++ ffExec
    killWindow w
    spawn $ "notify-send -t 3000 " ++ msg
    io . threadDelay $ delay*10^6
    spawn ffExec
    refresh
  doF id

doFull = do
  liftX . sendMessage $ removeStruts
  liftX . sendMessage $ JumpToLayout "full"
  doF id

doUnfloat = ask >>= doF . sink

addStruts = SetStruts [U,D,L,R] []
removeStruts = SetStruts [] [U,D,L,R]

doView workspace = doF $ view workspace
doShiftView workspace = doShift workspace <+> doView workspace




-- Helper functions to fullscreen the window
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ float w r
    where r = RationalRect 0 0 1 1
tileWin w = windows $ sink w

myEventHook :: Event -> X All
myEventHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win

  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2

      -- The ATOM property type for changeProperty
      ptype = 4

      action = head dat

  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win

  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False
myEventHook _ = return $ All True
