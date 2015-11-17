module WMLog (wmLogNew, WMLogConfig(..)) where
import Color (Color(..), widgetBgColor)
import Sep (sepW)
import Utils (fg, bg, fgbg)
import WorkspaceImages (loadImages, selectImage)

import Data.Maybe (fromMaybe)

import Graphics.UI.Gtk (
  Widget, WidgetClass, ContainerClass, escapeMarkup, widgetShowAll,
  toContainer, toWidget, hBoxNew, vBoxNew, frameNew, containerAdd,
  postGUIAsync)

import System.Taffybar.Pager (
  PagerConfig(..), Workspace(..), defaultPagerConfig,
  markWs, colorize, shorten, wrap, escape, pagerNew)
import System.Taffybar.LayoutSwitcher (layoutSwitcherNew)
import System.Taffybar.WindowSwitcher (windowSwitcherNew)
import System.Taffybar.WorkspaceSwitcher (wspaceSwitcherNew)
import System.Information.EWMHDesktopInfo (
  withDefaultCtx, getVisibleWorkspaces, getWindows, getWorkspace)

pagerConfig pixbufs cfg = defaultPagerConfig
  { activeWindow     = fg "#93a1a1" . escapeMarkup . fmtTitle cfg
  , activeLayout     = \x -> case x of
      "left"    -> return "[]="
      "top"     -> return "TTT"
      "grid"    -> return "###"
      "full"    -> do
        cnt <- windowCount
        let numFmt = if 0 <= cnt && cnt < 10 then show cnt else "+"
        let color = if cnt > 1 then fgbg "blue" "red" else id
        return $ color $ "[" ++ numFmt ++ "]"
      otherwise -> return $ fgbg "blue" "red" "???"
  , activeWorkspace  = wsStyle cfg (Just Red) $ bold . fgbg "#002b36" "#eee8d8"
  , hiddenWorkspace  = wsStyle cfg Nothing $ bold . fg "orange"
  , emptyWorkspace   = wsStyle cfg Nothing $ id
  , visibleWorkspace = wsStyle cfg Nothing $ id
  , urgentWorkspace  = markWs $ bold . fgbg "#002b36" "red" . escapeMarkup
  , hideEmptyWs      = False
  , wsButtonSpacing  = 3
  , widgetSep        = ""
  , imageSelector    = selectImage pixbufs
  , wrapWsButton     = wrapBorder $ wsBorderColor cfg
  }

windowCount :: IO Int
windowCount = withDefaultCtx $ do
  vis <- getVisibleWorkspaces
  let cur = if length vis > 0 then head vis else 0
  wins <- getWindows
  wkspaces <- mapM getWorkspace wins
  return $ length $ filter (==cur) $ wkspaces

data WMLogConfig = WMLogConfig { titleLength :: Int
                               , wsImageHeight :: Int
                               , titleRows :: Bool
                               , stackWsTitle :: Bool
                               , wsBorderColor :: Color
                               }

wsStyle cfg borderColor markupFct ws = do
  let col = fromMaybe (wsBorderColor cfg) borderColor
  postGUIAsync $ widgetBgColor col (wsContainer ws)
  markWs (markupFct . escapeMarkup) ws

wrapBorder color w = do
  f <- frameNew
  widgetBgColor color f
  containerAdd f w
  return $ toContainer f

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
  pixbufs <- loadImages $ wsImageHeight cfg
  pager <- pagerNew $ pagerConfig pixbufs cfg

  ws <- wspaceSwitcherNew pager
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
