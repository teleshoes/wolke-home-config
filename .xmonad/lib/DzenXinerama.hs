-- dzen xinerama visible/active workspace support
module DzenXinerama (dzenXineramaLogHook) where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook (readUrgents)
import XMonad.Util.Font (encodeOutput)
import XMonad.Util.NamedWindows (getName)

import qualified XMonad.StackSet as Stk

import Data.Function (on)
import Data.Maybe (isJust, catMaybes)
import Data.List (sortBy, intercalate)

import Control.Monad (zipWithM_)


dzenXineramaLogHook pps = do
 screens <- (sortBy (compare `on` Stk.screen) . Stk.screens)
            `fmap`
            gets windowset
 zipWithM_ dynamicLogWithPPScreen screens pps


dynamicLogWithPPScreen screen pp =
  dynamicLogStringScreen screen pp >>= io . ppOutput pp

dynamicLogStringScreen screen pp = do

  winset <- gets windowset
  urgents <- readUrgents
  sortF <- ppSort pp

  -- layout description
  let ld = description . Stk.layout . Stk.workspace $ screen

  -- workspace list
  let ws = pprWindowSetScreen screen sortF urgents pp winset

  -- window title
  wt <- maybe (return "") (fmap show . getName) $ focusedWindow screen

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (`catchX` return Nothing) $ ppExtras pp

  return $ encodeOutput . sepBy (ppSep pp) . ppOrder pp $
             [ ws
             , ppLayout pp ld
             , ppTitle pp wt
             ]
             ++ catMaybes extras

pprWindowSetScreen screen sortF urgents pp s =
  sepBy (ppWsSep pp) . map fmt . sortF $ Stk.workspaces s
    where this = Stk.tag . Stk.workspace $ screen
          visibles = map (Stk.tag . Stk.workspace) 
                         (Stk.current s : Stk.visible s)

          fmt w = printer pp (Stk.tag w)
              where printer | Stk.tag w == this = ppCurrent
                            | Stk.tag w `elem` visibles = ppVisible
                            | any (\x -> maybe False (== Stk.tag w)
                                                     (Stk.findTag x s))
                                  urgents = \ppC -> ppUrgent ppC . ppHidden ppC
                            | isJust (Stk.stack w) = ppHidden
                            | otherwise = ppHiddenNoWindows

focusedWindow = maybe Nothing (return . Stk.focus) . Stk.stack . Stk.workspace

sepBy :: String -> [String] -> String
sepBy sep = intercalate sep . filter (not . null)

