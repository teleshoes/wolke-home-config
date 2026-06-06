module LayoutWindows (layoutWindowsW) where
import Utils (fgbg)

import Data.Text (Text, pack, unpack)
import Control.Monad.Trans (liftIO)
import Safe (headDef)

import System.Taffybar.Widget.Layout (
  LayoutConfig(..), defaultLayoutConfig, layoutNew)
import System.Taffybar.Information.EWMHDesktopInfo (
  WorkspaceId(WorkspaceId), withX11Context, getVisibleWorkspaces, getWindows, getWorkspace)
import System.Taffybar.Information.X11DesktopInfo (DisplayName(..))

layoutWindowsW = layoutNew layoutConfig

layoutConfig = defaultLayoutConfig
  { formatLayout = \layout -> liftIO $ case unpack layout of
                       "left" -> return $ pack "[]="
                       "top"  -> return $ pack "TTT"
                       "grid" -> return $ pack "###"
                       "cols" -> return $ pack "|||"
                       "full" -> fmap formatWindowCount windowCount
                       otherwise -> return $ pack "???"
  }

formatWindowCount :: Int -> Text
formatWindowCount cnt = pack $ wcCol $ "[" ++ wcFmt ++ "]"
  where wcCol = if cnt > 1 then fgbg "blue" "red" else id
        wcFmt = if 0 <= cnt && cnt < 10 then show cnt else "+"

windowCount :: IO Int
windowCount = withX11Context (DisplayName "") $ do
  vis <- getVisibleWorkspaces
  let cur = headDef (WorkspaceId 0) vis
  wins <- getWindows
  wkspaces <- mapM getWorkspace wins
  return $ length $ filter (==cur) $ wkspaces
