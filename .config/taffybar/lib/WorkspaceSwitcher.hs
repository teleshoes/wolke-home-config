module WorkspaceSwitcher (workspaceSwitcherW) where
import Utils (fg, fgbg)
import WorkspaceImages (getIcon, loadIconInitialState)

import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe)

import Control.Monad.Trans (liftIO)

import System.Taffybar.Information.X11DesktopInfo (
  X11Property, X11Window, readAsListOfWindow)
import System.Taffybar.Widget.Workspaces (
  WindowData(..), WorkspacesConfig(..), WorkspacesIO, Workspace(..), WorkspaceState(..),
  defaultWorkspacesConfig, liftContext, liftX11Def, taffyContext, workspacesNew)

workspaceSwitcherW wsImageHeight = do
  iconInitialState <- liftIO $ loadIconInitialState $ wsImageHeight
  w <- workspacesNew $ workspacesConfig iconInitialState
  return w

workspacesConfig iconInitialState = defaultWorkspacesConfig
  { widgetGap           = 3
  , maxIcons            = Just 1
  , minIcons            = 1
  , iconSort            = sortWindowsByStackIndex
  , getWindowIconPixbuf = getIcon iconInitialState
  }

-- | Sort windows in reverse _NET_CLIENT_LIST_STACKING order.
-- Starting in xmonad-contrib 0.17.0, this is effectively focus history, active first.
-- Previous versions erroneously stored focus-sort-order in _NET_CLIENT_LIST.
sortWindowsByStackIndex :: [WindowData] -> WorkspacesIO [WindowData]
sortWindowsByStackIndex wins = do
  stackingWindows <- liftX11Def [] getWindowsStacking
  let getStackIdx wd = fromMaybe (-1) $ elemIndex (windowId wd) stackingWindows
      compareWindowData a b = compare (getStackIdx b) (getStackIdx a)
  return $ sortBy compareWindowData wins

-- | Return a list of all @X11Window@s, sorted in stacking order, bottom-to-top.
getWindowsStacking :: X11Property [X11Window]
getWindowsStacking = readAsListOfWindow Nothing "_NET_CLIENT_LIST_STACKING"
