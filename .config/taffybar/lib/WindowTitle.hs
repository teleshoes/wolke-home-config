module WindowTitle (windowTitleW) where
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Text (Text, pack)
import System.Taffybar.Context (runX11)
import System.Taffybar.Information.EWMHDesktopInfo (getActiveWindow, getWindowTitle)
import System.Taffybar.Widget.Windows (
  WindowsConfig(..), defaultWindowsConfig, windowsNew)

windowTitleW len lineCount = windowsNew $ defaultWindowsConfig
  { getActiveLabel = fmap (formatTitle len lineCount) $ runX11 getActiveWinTitle
  }

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
