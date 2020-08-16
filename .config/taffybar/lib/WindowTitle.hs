module WindowTitle (windowTitleW) where
import Width (widthCharWrapW, getScreenDPI)
import Utils (sleep, trimR, padR)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Text (Text, pack)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Widget (toWidget, widgetShowAll)
import System.Taffybar.Context (runX11)
import System.Taffybar.Information.EWMHDesktopInfo (getActiveWindow, getWindowTitle)
import System.Taffybar.Information.X11DesktopInfo (getDefaultCtx)
import System.Taffybar.Widget.Windows (
  WindowsConfig(..), defaultWindowsConfig, windowsNew)

main = do
  let profileTitle = 30
  let linesInBar = 2
  ctx <- getDefaultCtx
  forever $ do
    winTitle <- runReaderT getActiveWinTitle ctx
    print $ formatTitle profileTitle linesInBar winTitle
    sleep 1

windowTitleW fontSizePt len lineCount = do
  dpi <- lift getScreenDPI
  let config = defaultWindowsConfig
               { getActiveLabel = fmap (formatTitle len lineCount) $ runX11 getActiveWinTitle
               }
  windowsW <- windowsNew config
  lift $ widthCharWrapW dpi fontSizePt len windowsW

getActiveWinTitle = do
  w <- getActiveWindow
  case w of
    Just window -> getWindowTitle window
    Nothing -> return ""

formatTitle :: Int -> Int -> String -> Text
formatTitle len lineCount title = pack $ intercalate "\n" $ map (padTrim len) $ lines
  where lines = take lineCount $ chunksOf len title ++ repeat ""

padTrim :: Int -> String -> String
padTrim n x = padR ' ' n $ trimR n x
