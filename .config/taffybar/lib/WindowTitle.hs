module WindowTitle (windowTitleW) where
import Utils (fg)
import Label (labelW)

import Graphics.UI.Gtk (escapeMarkup, widgetShowAll)
import Control.Monad.Trans (liftIO, lift)
import System.Taffybar.Context (runX11)
import System.Taffybar.Information.EWMHDesktopInfo (getActiveWindowTitle, withDefaultCtx)
import System.Taffybar.Widget.Windows (
  WindowsConfig(..), defaultWindowsConfig, windowsNew)

windowTitleW len useRows = wrapWidgetShowAll $ windowsNew $ defaultWindowsConfig
  { getActiveLabel = fmap (markupTitleText len useRows) $ runX11 getActiveWindowTitle
  }

wrapWidgetShowAll getWidget = do
  w <- getWidget
  liftIO $ widgetShowAll w
  return w

markupTitleText :: Int -> Bool -> String -> String
markupTitleText len useRows = fg "#93a1a1" . escapeMarkup . formatTitleText len useRows

formatTitleText :: Int -> Bool -> String -> String
formatTitleText len useRows t = if useRows then rows else padTrim len t
  where rows = (padTrim len top) ++ "\n" ++ (padTrim len bot)
        (top, bot) = splitAt len t

padTrim :: Int -> String -> String
padTrim n x = take n $ x ++ repeat ' '
