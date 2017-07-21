module WMLog (wmLogNew, WMLogConfig(..)) where
import Color (Color(..))
import Sep (sepW)
import Utils (fg, fgbg)
import WorkspaceImages (getIcons, selectImage)

import Data.Maybe (fromMaybe)

import Graphics.UI.Gtk (
  Widget, WidgetClass, ContainerClass, escapeMarkup, widgetShowAll,
  toWidget, hBoxNew, vBoxNew, containerAdd)

import System.Taffybar.Pager (
  PagerConfig(..), defaultPagerConfig,
  colorize, shorten, wrap, escape, pagerNew)
import System.Taffybar.LayoutSwitcher (layoutSwitcherNew)
import System.Taffybar.WindowSwitcher (windowSwitcherNew)
import System.Taffybar.WorkspaceSwitcher (wspaceSwitcherNew)
import System.Taffybar.WorkspaceHUD (hudFromPagerConfig, buildWorkspaceHUD)
import System.Information.EWMHDesktopInfo (
  WorkspaceIdx(WSIdx), withDefaultCtx, getVisibleWorkspaces, getWindows, getWorkspace)

pagerConfig icons cfg = defaultPagerConfig
  { activeWindow     = fg "#93a1a1" . escapeMarkup . fmtTitle cfg
  , activeLayoutIO   = \layout -> case layout of
                         "left" -> return "[]="
                         "top"  -> return "TTT"
                         "grid" -> return "###"
                         "full" -> fmap formatWindowCount windowCount
  , activeWorkspace  = bold . fgbg "#002b36" "#eee8d8"
  , visibleWorkspace = bold . fgbg "#002b36" "#eee8d8"
  , urgentWorkspace  = bold . fgbg "#002b36" "red"
  , hiddenWorkspace  = bold . fg "orange"
  , emptyWorkspace   = id
  , workspaceBorder  = True
  , workspaceGap     = 3
  , workspacePad     = False
  , widgetSep        = ""
  , useImages        = True
  , imageSize        = wsImageHeight cfg
  , fillEmptyImages  = True
  , preferCustomIcon = False
  , customIcon       = selectImage icons
  }

formatWindowCount :: Int -> String
formatWindowCount cnt = wcCol $ "[" ++ wcFmt ++ "]"
  where wcCol = if cnt > 1 then fgbg "blue" "red" else id
        wcFmt = if 0 <= cnt && cnt < 10 then show cnt else "+"

windowCount :: IO Int
windowCount = withDefaultCtx $ do
  vis <- getVisibleWorkspaces
  let cur = if length vis > 0 then head vis else WSIdx 0
  wins <- getWindows
  wkspaces <- mapM getWorkspace wins
  return $ length $ filter (==cur) $ wkspaces

data WMLogConfig = WMLogConfig { titleLength :: Int
                               , wsImageHeight :: Int
                               , titleRows :: Bool
                               , stackWsTitle :: Bool
                               }

bold m = "<b>" ++ m ++ "</b>"

padTrim n x = take n $ x ++ repeat ' '

fmtTitle cfg t = if titleRows cfg then rows else padTrim len t
  where rows = (padTrim len top) ++ "\n" ++ (padTrim len bot)
        (top, bot) = splitAt len t
        len = titleLength cfg

box :: ContainerClass c => WidgetClass w => IO c -> [IO w] -> IO Widget
box c ws = do
  container <- c
  mapM (containerAdd container) =<< sequence ws
  return $ toWidget container

wmLogNew cfg = do
  icons <- getIcons $ wsImageHeight cfg
  let pgrCfg = pagerConfig icons cfg
  pager <- pagerNew pgrCfg

  ws <- buildWorkspaceHUD (hudFromPagerConfig pgrCfg) pager
  --ws <- wspaceSwitcherNew pager
  title <- windowSwitcherNew pager
  layout <- layoutSwitcherNew pager

  w <- box (hBoxNew False 3) $
       if stackWsTitle cfg then
         [ box (vBoxNew False 0)
           [ return ws
           , return title
           ]
         , return layout
         ]
       else
         [ return ws
         , return title
         , sepW Black 2
         , return layout
         ]
  widgetShowAll w
  return w
