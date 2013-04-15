module WMLog (wmLogNew) where
import Sep (sepW)
import Utils (fg, bg, fgbg)
import WorkspaceImages (loadImages, selectImage)

import System.Taffybar.WorkspaceSwitcher (wspaceSwitcherNew)

import Graphics.UI.Gtk (
  Widget, WidgetClass, ContainerClass, escapeMarkup, widgetShowAll,
  castToContainer, toWidget, frameNew,
  hBoxNew, vBoxNew, containerAdd, containerForeach, containerGetChildren,
  Color(..), StateType(..), widgetModifyBg)

import System.Taffybar.Pager (
  PagerConfig(..), defaultPagerConfig,
  colorize, shorten, wrap, escape, pagerNew)
import System.Taffybar.LayoutSwitcher (layoutSwitcherNew)
import System.Taffybar.WindowSwitcher (windowSwitcherNew)

wsBorderColor = Color 40000 30000 10000


pagerConfig pixbufs titleRows titleLen = defaultPagerConfig
  { activeWindow     = fg "green" . escapeMarkup . fmtTitle titleRows titleLen
  , activeLayout     = \x -> case x of
                               "left"    ->                   "[]="
                               "top"     -> fgbg "blue" "red" "TTT"
                               "full"    -> fgbg "blue" "red" "[ ]"
                               otherwise -> fgbg "blue" "red" "???"
  , activeWorkspace  = bold . fgbg "black" "green" . escapeMarkup
  , hiddenWorkspace  = bold . fg "orange" . escapeMarkup
  , emptyWorkspace   = escapeMarkup
  , visibleWorkspace = escapeMarkup
  , urgentWorkspace  = bold . fgbg "red" "yellow" . escapeMarkup
  , imageSelector    = selectImage pixbufs
  , widgetSep        = ""
  }
bold m = "<b>" ++ m ++ "</b>"

padTrim n x = take n $ x ++ repeat ' '

fmtTitle rows len t = if rows then titleRows else padTrim len t
  where titleRows = (padTrim len top) ++ "\n" ++ (padTrim len bot)
        (top, bot) = splitAt len t

applyToGrandChildren container cb = do
  children <- containerGetChildren $ castToContainer container
  mapM (\child -> containerForeach child cb) $ map castToContainer children

setWsBorderColor w = widgetModifyBg w StateNormal wsBorderColor

box :: ContainerClass c => WidgetClass w => IO c -> [IO w] -> IO Widget
box c ws = do
  container <- c
  mapM (containerAdd container) =<< sequence ws
  return $ toWidget container

wmLogNew titleLength imageHeight titleRows stackWsTitle = do
  pixbufs <- loadImages imageHeight
  pager <- pagerNew $ pagerConfig pixbufs titleRows titleLength

  ws <- wspaceSwitcherNew pager
  title <- windowSwitcherNew pager
  layout <- layoutSwitcherNew pager

  applyToGrandChildren ws setWsBorderColor

  w <- box (hBoxNew False 3) $
       if stackWsTitle then
         [ box (vBoxNew False 0)
           [ return ws
           , return title
           ]
         , return layout
         ]
       else
         [ return ws
         , return title
         , sepW
         , return layout
         ]
  widgetShowAll w
  return w
