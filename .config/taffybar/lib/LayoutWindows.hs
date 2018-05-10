module LayoutWindows (layoutWindowsW) where
import Utils (fgbg)

import Control.Monad.Trans (liftIO)

import System.Taffybar.Widget.Layout (
  LayoutConfig(..), defaultLayoutConfig, layoutNew)
import System.Taffybar.Information.EWMHDesktopInfo (
  WorkspaceIdx(WSIdx), withDefaultCtx, getVisibleWorkspaces, getWindows, getWorkspace)
import System.Taffybar.Compat.GtkLibs (fromGIWidget)

layoutWindowsW = layoutNew layoutConfig

layoutConfig = defaultLayoutConfig
  { formatLayout = \layout -> liftIO $ case layout of
                       "left" -> return "[]="
                       "top"  -> return "TTT"
                       "grid" -> return "###"
                       "full" -> fmap formatWindowCount windowCount
  }

formatWindowCount :: Int -> String
formatWindowCount cnt = wcCol $ "[" ++ wcFmt ++ "]"
  where wcCol = if cnt > 1 then fgbg "blue" "red" else id
        wcFmt = if 0 <= cnt && cnt < 10 then show cnt else "+"

windowCount :: IO Int
windowCount = withDefaultCtx $ do
  vis <- getVisibleWorkspaces
  let cur = if length vis > 0 then head vis else WSIdx 0
  wins <- getWindows
  wkspaces <- mapM getWorkspace wins
  return $ length $ filter (==cur) $ wkspaces
