module WindowTitle (windowTitleW) where
import System.Taffybar.Widget.Util (widgetSetClassGI)
import Control.Monad.Trans.Reader (runReaderT)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Text (Text, pack)
import GI.Gtk.Enums (PolicyType(..))
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.ScrolledWindow (scrolledWindowNew, scrolledWindowSetPolicy)
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
  winTitle <- runReaderT getActiveWinTitle ctx
  print $ formatTitle profileTitle linesInBar winTitle

windowTitleW len lineCount = wrapFixedHeightPanel =<< windowsNew config
  where config = defaultWindowsConfig { getActiveLabel = getFmtWinTitle }
        getFmtWinTitle = fmap (formatTitle len lineCount) $ runX11 getActiveWinTitle

wrapFixedHeightPanel childW = do
  scroll <- scrolledWindowNew noAdjustment noAdjustment
  containerAdd scroll childW
  scrolledWindowSetPolicy scroll PolicyTypeNever PolicyTypeExternal
  widgetShowAll scroll
  toWidget scroll

getActiveWinTitle = do
  w <- getActiveWindow
  case w of
    Just window -> getWindowTitle window
    Nothing -> return ""

formatTitle :: Int -> Int -> String -> Text
formatTitle len lineCount title = pack $ intercalate "\n" $ map (padTrim len) $ lines
  where lines = take lineCount $ chunksOf len title ++ repeat ""

padTrim :: Int -> String -> String
padTrim n x = take n $ x ++ repeat ' '
