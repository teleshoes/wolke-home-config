module WMLog (wmLogNew, WMLogConfig(..)) where
import Color (Color(..), widgetBgColor)
import Sep (sepW)
import Utils (fg, bg, fgbg)
import WorkspaceImages (loadImages, selectImage)

import System.Taffybar.WorkspaceSwitcher (wspaceSwitcherNew)

import Graphics.UI.Gtk (
  Widget, WidgetClass, ContainerClass, escapeMarkup, widgetShowAll,
  toContainer, toWidget, hBoxNew, vBoxNew, frameNew, containerAdd)

import System.Taffybar.Pager (
  PagerConfig(..), defaultPagerConfig,
  markWs, colorize, shorten, wrap, escape, pagerNew)
import System.Taffybar.LayoutSwitcher (layoutSwitcherNew)
import System.Taffybar.WindowSwitcher (windowSwitcherNew)

data WMLogConfig = WMLogConfig { titleLength :: Int
                               , wsImageHeight :: Int
                               , titleRows :: Bool
                               , stackWsTitle :: Bool
                               , wsBorderColor :: Color
                               }

pagerConfig pixbufs cfg = defaultPagerConfig
  { activeWindow     = fg "green" . escapeMarkup . fmtTitle cfg
  , activeLayout     = \x -> case x of
                               "left"    ->                   "[]="
                               "top"     -> fgbg "blue" "red" "TTT"
                               "full"    -> fgbg "blue" "red" "[ ]"
                               otherwise -> fgbg "blue" "red" "???"
  , activeWorkspace  = markWs $ bold . fgbg "black" "green" . escapeMarkup
  , hiddenWorkspace  = markWs $ bold . fg "orange" . escapeMarkup
  , emptyWorkspace   = markWs $ escapeMarkup
  , visibleWorkspace = markWs $ escapeMarkup
  , urgentWorkspace  = markWs $ bold . fgbg "red" "yellow" . escapeMarkup
  , hideEmptyWs      = False
  , wsButtonSpacing  = 3
  , widgetSep        = ""
  , imageSelector    = selectImage pixbufs
  , wrapWsButton     = wrapBorder $ wsBorderColor cfg
  }

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
