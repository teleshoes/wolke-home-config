module WindowTitle (windowTitleW) where
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import System.Taffybar.Context (runX11)
import System.Taffybar.Information.EWMHDesktopInfo (getActiveWindowTitle)
import System.Taffybar.Widget.Windows (
  WindowsConfig(..), defaultWindowsConfig, windowsNew)

windowTitleW len lineCount = windowsNew $ defaultWindowsConfig
  { getActiveLabel = fmap (formatTitle len lineCount) $ runX11 getActiveWindowTitle
  }

formatTitle :: Int -> Int -> String -> String
formatTitle len lineCount title = intercalate "\n" $ map (padTrim len) $ lines
  where lines = take lineCount $ chunksOf len title ++ repeat ""

padTrim :: Int -> String -> String
padTrim n x = take n $ x ++ repeat ' '
