-- dzen xinerama visible/active workspace support
module DzenXinerama (dzenXineramaLogHook) where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook (readUrgents)
import XMonad.Util.Font (encodeOutput)
import XMonad.Util.NamedWindows (getName)

import qualified XMonad.StackSet as Stk

import Data.Function (on)
import Data.Maybe (isJust, catMaybes, fromMaybe, listToMaybe)
import Data.List (sortBy, intercalate)
import XMonad.Util.Run (safeSpawn)
import Control.Monad (zipWithM_)
import System.IO (hGetLine)

import Control.Applicative ( (<$>) )

import Prelude hiding ( catch )
import Control.Exception.Extensible ( bracket, catch, SomeException(..) )

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

  let curWin = focusedWindow screen
  -- workspaceId
  let workspaceId = Stk.tag . Stk.workspace $ screen
  -- workspace list
  let workspaceList = pprWindowSetScreen screen sortF urgents pp winset

  -- window class
  windowClass <- getCurWinProp windowClass curWin
  -- window title
  windowName <- getCurWinProp windowName curWin

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (`catchX` return Nothing) $ ppExtras pp

  --translates the window title to an image and writes to file
  _ <- safeSpawn "workspace-image" [workspaceId, windowClass, windowName]

  return $ encodeOutput . sepBy (ppSep pp) . ppOrder pp $
             [ workspaceList
             , ppLayout pp ld
             , ppTitle pp windowName
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

getCurWinProp xProp curWin = maybe (return "") (getProp xProp) curWin

focusedWindow = maybe Nothing (return . Stk.focus) . Stk.stack . Stk.workspace

sepBy :: String -> [String] -> String
sepBy sep = intercalate sep . filter (not . null)


windowName = wM_NAME
windowClass = wM_CLASS

getProp :: Atom -> Window -> X String
getProp wProp w = withDisplay $ \d -> do
    let getIt = bracket getProp (xFree . tp_value) (copy)
        getProp = getTextProperty d w wProp
        copy prop = fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop
    io $ getIt `catch` \(SomeException _) ->  (resName) `fmap` getClassHint d w

