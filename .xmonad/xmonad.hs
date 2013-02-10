{-# LANGUAGE TemplateHaskell #-}
import Bindings (myKeyBindings, myMouseBindings, workspaceNames, mouseOverlaps, keyOverlaps)
import Dzen (spawnHookedDzens, spawnUnhookedDzens, myDzenLogHook)
import StaticAssert (staticAssert)

import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators ( (|||), JumpToLayout(..))

import XMonad.Hooks.ManageDocks (avoidStruts, SetStruts(..))
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Types (Direction2D(U,D,L,R))
import System.Environment.UTF8 (getEnv)

import qualified XMonad.StackSet as Stk

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Writer
import Data.Monoid (All(All))

staticAssert (null mouseOverlaps && null keyOverlaps) . execWriter $ do
    tell "Error: Overlap in bindings\n"
    let pretty = tell . unlines . map ((replicate 8 ' ' ++) . show . map fst)
    pretty mouseOverlaps
    pretty keyOverlaps

firefoxExec = "firefox"
firefoxProcess = "firefox"
firefoxClose = "Close Firefox"

main = xmonad =<< myConfig <$> getEnv "HOME" <*> spawnDzens

myConfig home dzens = defaultConfig
  { focusFollowsMouse  = False
  , modMask            = mod1Mask
  , normalBorderColor  = "#dddddd"
  , focusedBorderColor = "#ff0000"
  , borderWidth        = 3

  , logHook            = myDzenLogHook home workspaceNames dzens
  , startupHook        = myStartupHook
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook

  , workspaces         = workspaceNames
  , keys               = myKeyBindings
  , mouseBindings      = myMouseBindings

  , handleEventHook    = myHandleEventHook
  -- , terminal           =
  }

spawnDzens = do
    safeSpawn "workspace-image" ("init":workspaceNames)
    spawn "killall dzen2 2>/dev/null"
    spawnUnhookedDzens
    spawnHookedDzens

myStartupHook = spawn "find $HOME/.xmonad/ -regex '.*\\.\\(hi\\|o\\)' -delete"

myLayoutHook = avoidStruts . smartBorders
             $   named "left" (Tall 1 incr ratio)
             ||| named "top"  (Mirror $ Tall 1 incr ratio)
             ||| named "full" Full
  where incr = 3/100 ; ratio = 55/100

myManageHook = execWriter $ do
  let a ~~> b = tell (a --> b)
  title     =? "Find/Replace "         ~~> doFloat
  className =? "Eclipse"               ~~> (doShift "A" <+> doUnfloat)
  title     =? "GWT Development Mode"  ~~> doShift "G"
  className =? "Pidgin"                ~~> doShift "B"
  className =? "Thunderbird"           ~~> doShift "8"
  title     =? "Off"                   ~~> doFloat
  title     =? "KLOMP"                 ~~> doShift "9"
  title     =? "Transmission"          ~~> doShift "9"
  title     =? "Torrent Options"       ~~> doShiftView "9"
  title     =? firefoxClose            ~~> restartFF
  title     =? "qtbigtext.py"          ~~> doFull
  title     =? "StepMania"             ~~> doFull
  title     =? "npviewer.bin"          ~~> doFull -- flash
  title     =? "plugin-container"      ~~> doFull -- flash
  title     =? "xfce4-notifyd"         ~~> doIgnore

myHandleEventHook _ = return (All True)

restartFF = do
  w <- ask
  let delay = 1
  let msg = "'restarting " ++ firefoxExec ++ " in " ++ show delay ++ "s'"
  liftX $ do
    spawn $ "killall -9 " ++ firefoxProcess
    killWindow w
    spawn $ "notify-send -t 3000 " ++ msg
    io . threadDelay $ delay*10^6
    spawn firefoxExec
    refresh
  doF id

doFull = do
  liftX . sendMessage $ removeStruts
  liftX . sendMessage $ JumpToLayout "full"
  doF id

doUnfloat = ask >>= doF . Stk.sink

addStruts = SetStruts [U,D,L,R] []
removeStruts = SetStruts [] [U,D,L,R]

doView workspace = doF $ Stk.view workspace
doShiftView workspace = doShift workspace <+> doView workspace
