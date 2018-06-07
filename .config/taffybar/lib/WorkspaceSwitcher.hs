module WorkspaceSwitcher (workspaceSwitcherW) where
import Utils (fg, fgbg)
import WorkspaceImages (getIcon, loadNamedIconFileMap, loadPlaceholderPixbuf)

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk (Widget)

import System.Taffybar.Widget.Workspaces (
  WorkspacesConfig(..), WorkspacesIO, Workspace(..), WorkspaceState(..),
  defaultWorkspacesConfig, workspacesNew)

workspaceSwitcherW wsImageHeight = do
  namedIconFileMap <- liftIO $ loadNamedIconFileMap $ wsImageHeight
  placeholderPixbuf <- liftIO $ loadPlaceholderPixbuf wsImageHeight
  w <- workspacesNew $ workspacesConfig namedIconFileMap wsImageHeight placeholderPixbuf
  return w

workspacesConfig namedIconFileMap wsImageHeight placeholderPixbuf = defaultWorkspacesConfig
  { widgetGap           = 3
  , maxIcons            = Just 1
  , minIcons            = 1
  , iconSort            = return -- unsorted leaves active first
  , getWindowIconPixbuf = getIcon namedIconFileMap wsImageHeight placeholderPixbuf
  }
