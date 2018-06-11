module WorkspaceSwitcher (workspaceSwitcherW) where
import Utils (fg, fgbg)
import WorkspaceImages (getIcon, loadIconInitialState)

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk (Widget)

import System.Taffybar.Widget.Workspaces (
  WorkspacesConfig(..), WorkspacesIO, Workspace(..), WorkspaceState(..),
  defaultWorkspacesConfig, workspacesNew)

workspaceSwitcherW wsImageHeight = do
  iconInitialState <- liftIO $ loadIconInitialState $ wsImageHeight
  w <- workspacesNew $ workspacesConfig iconInitialState
  return w

workspacesConfig iconInitialState = defaultWorkspacesConfig
  { widgetGap           = 3
  , maxIcons            = Just 1
  , minIcons            = 1
  , iconSort            = return -- unsorted leaves active first
  , getWindowIconPixbuf = getIcon iconInitialState
  }
