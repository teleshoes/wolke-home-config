{-# LANGUAGE TemplateHaskell #-}
import Bindings
import Bindings.Writer
import StaticAssert

import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators ( (|||), JumpToLayout(..))

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, SetStruts(..), manageDocks)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Types (Direction2D(U,D,L,R))

import qualified XMonad.StackSet as Stk

import System.Taffybar.Hooks.PagerHints (pagerHints)

import Control.Concurrent (threadDelay)

staticAssert (null mouseOverlaps && null keyOverlaps) . execWriter $ do
    tell "Error: Overlap in bindings\n"
    let pretty = tell . unlines . map ((replicate 8 ' ' ++) . show . map fst)
    pretty mouseOverlaps
    pretty keyOverlaps

main = xmonad . ewmh . pagerHints $ defaultConfig
    { focusFollowsMouse  = False
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#ff0000"
    , borderWidth        = 2

    , startupHook        = myStartupHook
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook <+> manageDocks

    , workspaces         = workspaceNames
    , keys               = myKeyBindings
    , mouseBindings      = myMouseBindings

    -- , logHook            =
    -- , terminal           =
    -- , handleEventHook    =
    -- , modMask            =
    }

myStartupHook = return ()

myLayoutHook = avoidStruts . smartBorders
             $   named "left" (Tall 1 incr ratio)
             ||| named "top"  (Mirror $ Tall 1 incr ratio)
             ||| named "full" Full
  where incr = 5/100 ; ratio = 50/100

infixr 0 ~~>
a ~~> b = tell (a --> b)

myManageHook = execWriter $ do
    title =? "Close Iceweasel"   ~~> restartFF
    title =? "Close Firefox"     ~~> restartFF
    title =? "npviewer.bin"      ~~> doFull
    title =? "plugin-container"  ~~> doFull

restartFF = do
    w <- ask
    let delay = 1
    liftX $ do
        killWindow w
        io . threadDelay $ delay*10^6
        let msg = "'restarting firefox in " ++ show delay ++ "s'"
        spawn $ "notify-send -t 3000 " ++ msg
        spawn "firefox"
        refresh
    doF id

doFull = do
    liftX . sendMessage $ removeStruts
    liftX . sendMessage $ JumpToLayout "full"
    doF id
    -- TODO: add layout to extensible state,
    --       add event hook to restore layout when window closes
    --       first step, just restore defaut layout then window closes


addStruts = SetStruts [U,D,L,R] []
removeStruts = SetStruts [] [U,D,L,R]

doView workspace = doF $ Stk.view workspace
doShiftView workspace = doShift workspace <+> doView workspace
