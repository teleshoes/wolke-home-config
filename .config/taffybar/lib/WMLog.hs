module WMLog (wmLogNew) where
import WorkspaceSwitcherImages (workspaceSwitcherImagesNew)
import Utils (fg, fgbg)

import Graphics.UI.Gtk (
  escapeMarkup, toWidget, hBoxNew, vBoxNew, containerAdd, widgetShowAll)

import System.Taffybar.Pager (
  PagerConfig(..), defaultPagerConfig,
  colorize, shorten, wrap, escape, pagerNew)
import System.Taffybar.LayoutSwitcher (layoutSwitcherNew)
import System.Taffybar.WindowSwitcher (windowSwitcherNew)

titleLength = 60

bold m = "<b>" ++ m ++ "</b>"
padTrim n x = take n $ x ++ repeat ' '

pagerConfig = defaultPagerConfig
  { activeWindow     = fg "green" . escapeMarkup . padTrim titleLength
  , activeLayout     = \x -> case x of
                               "left"    ->                   "[]="
                               "top"     -> fgbg "blue" "red" "TTT"
                               "full"    -> fgbg "blue" "red" "[ ]"
                               otherwise -> fgbg "blue" "red" "???"
  , activeWorkspace  = bold . fg "yellow" . escapeMarkup
  , visibleWorkspace = escapeMarkup
  , urgentWorkspace  = colorize "red" "yellow" . escapeMarkup
  , widgetSep        = ""
  }

wmLogNew = do
  pager <- pagerNew pagerConfig
  ws <- workspaceSwitcherImagesNew pager
  title <- windowSwitcherNew pager
  layout <- layoutSwitcherNew pager

  wsTitleBox <- vBoxNew True 0
  containerAdd wsTitleBox ws
  containerAdd wsTitleBox title

  wmLogBox <- hBoxNew False 5
  containerAdd wmLogBox wsTitleBox
  containerAdd wmLogBox layout

  widgetShowAll wmLogBox

  return $ toWidget wmLogBox
