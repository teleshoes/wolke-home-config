module WorkspaceSwitcher (workspaceSwitcherW) where
import Utils (fg, fgbg)
import WorkspaceImages (getIcon, loadIconInitialState)

import Control.Monad.Trans (liftIO)

import System.Taffybar.Widget.Workspaces (
  WorkspacesConfig(..), WorkspacesIO, Workspace(..), WorkspaceState(..),
  defaultWorkspacesConfig, sortWindowsByPosition, workspacesNew)

workspaceSwitcherW wsImageHeight = do
  iconInitialState <- liftIO $ loadIconInitialState $ wsImageHeight
  w <- workspacesNew $ workspacesConfig iconInitialState
  return w

workspacesConfig iconInitialState = defaultWorkspacesConfig
  { widgetGap           = 3
  , maxIcons            = Just 1
  , minIcons            = 1
  , iconSort            = sortWindowsByPosition --unsorted is no longer last-active
  , getWindowIconPixbuf = getIcon iconInitialState
  }
