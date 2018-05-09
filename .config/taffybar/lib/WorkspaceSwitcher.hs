module WorkspaceSwitcher (workspaceSwitcherW) where
import Utils (fg, fgbg)
import WorkspaceImages (getIcon, loadNamedIconFileMap)

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk (Widget)

import System.Taffybar.Widget.Workspaces (
  WorkspacesConfig(..), WorkspacesIO, Workspace(..), WorkspaceState(..),
  defaultWorkspacesConfig, workspacesNew,
  defaultGetIconInfo)

workspaceSwitcherW wsImageHeight = do
  namedIconFileMap <- liftIO $ loadNamedIconFileMap $ wsImageHeight
  w <- workspacesNew $ workspacesConfig namedIconFileMap wsImageHeight
  return w

workspacesConfig namedIconFileMap wsImageHeight = defaultWorkspacesConfig
  { labelSetter      = formatLabelIO
  , widgetGap        = 3
  , maxIcons         = Just 1
  , minIcons         = 1
  , windowIconSize   = wsImageHeight
  , iconSort         = return -- unsorted leaves active first
  , getIconInfo      = getIcon namedIconFileMap
  }

formatLabelIO :: Workspace -> WorkspacesIO String
formatLabelIO ws = return $ formatLabel (workspaceName ws) (workspaceState ws)

formatLabel wsName wsState = case wsState of
  Active  -> bold $ fgbg "#002b36" "#eee8d8" $ wsName
  Visible -> bold $ fgbg "#002b36" "#eee8d8" $ wsName
  Hidden  -> bold $ fg "orange" $ wsName
  Urgent  -> bold $ fgbg "#002b36" "red" $ wsName
  Empty   -> wsName

bold m = "<b>" ++ m ++ "</b>"
