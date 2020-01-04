module LayoutWindows (layoutWindowsW) where
import Utils (fgbg)

import Data.Text (Text, pack, unpack)
import Control.Monad.Trans (liftIO)

import System.Taffybar.Widget.Layout (
  LayoutConfig(..), defaultLayoutConfig, layoutNew)
import System.Taffybar.Information.EWMHDesktopInfo (
  WorkspaceId(WorkspaceId), withDefaultCtx, getVisibleWorkspaces, getWindows, getWorkspace)

layoutWindowsW = layoutNew layoutConfig

layoutConfig = defaultLayoutConfig
  { formatLayout = \layout -> liftIO $ case unpack layout of
                       "left" -> return $ pack "[]="
                       "top"  -> return $ pack "TTT"
                       "grid" -> return $ pack "###"
                       "full" -> fmap formatWindowCount windowCount
  }

formatWindowCount :: Int -> Text
formatWindowCount cnt = pack $ wcCol $ "[" ++ wcFmt ++ "]"
  where wcCol = if cnt > 1 then fgbg "blue" "red" else id
        wcFmt = if 0 <= cnt && cnt < 10 then show cnt else "+"

windowCount :: IO Int
windowCount = withDefaultCtx $ do
  vis <- getVisibleWorkspaces
  let cur = if length vis > 0 then head vis else WorkspaceId 0
  wins <- getWindows
  wkspaces <- mapM getWorkspace wins
  return $ length $ filter (==cur) $ wkspaces
