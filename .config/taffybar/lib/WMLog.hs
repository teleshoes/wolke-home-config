module WMLog (wmLogNew) where
import WorkspaceImages (loadImages, selectImage)
import System.Taffybar.WorkspaceSwitcher (wspaceSwitcherNew)
import Utils (fg, bg, fgbg)

import Graphics.UI.Gtk (
  escapeMarkup, widgetShowAll,
  castToContainer, toWidget,
  hBoxNew, vBoxNew, containerAdd, containerForeach, containerGetChildren,
  Color(..), StateType(..), widgetModifyBg)

import System.Taffybar.Pager (
  PagerConfig(..), defaultPagerConfig,
  colorize, shorten, wrap, escape, pagerNew)
import System.Taffybar.LayoutSwitcher (layoutSwitcherNew)
import System.Taffybar.WindowSwitcher (windowSwitcherNew)

wsBorderColor = Color 65535 0 0

titleLength = 60

bold m = "<b>" ++ m ++ "</b>"
padTrim n x = take n $ x ++ repeat ' '

pagerConfig pixbufs = defaultPagerConfig
  { activeWindow     = fg "green" . escapeMarkup . padTrim titleLength
  , activeLayout     = \x -> case x of
                               "left"    ->                   "[]="
                               "top"     -> fgbg "blue" "red" "TTT"
                               "full"    -> fgbg "blue" "red" "[ ]"
                               otherwise -> fgbg "blue" "red" "???"
  , activeWorkspace  = bold . fgbg "black" "green" . escapeMarkup
  , hiddenWorkspace  = fg "orange" . escapeMarkup
  , emptyWorkspace   = escapeMarkup
  , visibleWorkspace = escapeMarkup
  , urgentWorkspace  = fgbg "red" "yellow" . escapeMarkup
  , imageSelector    = selectImage pixbufs
  , widgetSep        = ""
  }

applyToGrandChildren container cb = do
  children <- containerGetChildren $ castToContainer container
  mapM (\child -> containerForeach child cb) $ map castToContainer children

setWsBorderColor w = widgetModifyBg w StateNormal wsBorderColor

wmLogNew = do
  pixbufs <- loadImages
  pager <- pagerNew $ pagerConfig pixbufs
  ws <- wspaceSwitcherNew pager
  title <- windowSwitcherNew pager
  layout <- layoutSwitcherNew pager

  applyToGrandChildren ws setWsBorderColor

  wsTitleBox <- vBoxNew True 0
  containerAdd wsTitleBox ws
  containerAdd wsTitleBox title

  wmLogBox <- hBoxNew False 5
  containerAdd wmLogBox wsTitleBox
  containerAdd wmLogBox layout

  widgetShowAll wmLogBox

  return $ toWidget wmLogBox
