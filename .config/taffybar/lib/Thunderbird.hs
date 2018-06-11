module Thunderbird(thunderbirdW) where
import Color (Color(..), hexColor, widgetBgColorWrap)
import Clickable (clickable)
import Image (imageW)
import Label (labelW, mainLabel)
import Graphics.UI.Gtk (containerAdd, hBoxNew)
import Utils (
  imageDir, fg, getHome, regexGroups, chompAll, padL, isRunning, readProc, chompFile)

import qualified Data.Map as M (fromList, lookup, member)

import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getEnv)

main = mainLabel $ unreadCountsMarkup $ hexColor White
thunderbirdW h fgColor bgColor = do
  img <- imageW (getImage h)
  label <- labelW $ unreadCountsMarkup $ hexColor fgColor

  box <- hBoxNew False 0
  containerAdd box img
  containerAdd box label

  widgetBgColorWrap bgColor =<< clickable clickL clickM clickR box


exec = "icedove"
process = exec
dir = "." ++ exec

clickL = Just $ exec
clickM = Just $ exec ++ " --compose"
clickR = Just $ "pkill " ++ process

accounts = M.fromList [ ("Gmail", "G")
                      , ("LilleGroup", "L")
                      , ("AOL - LiberiFataliVIII", "A")
                      , ("teleshoes", "T")
                      ]

getImage h = do
  tbRunning <- isRunning process
  dir <- imageDir h
  let img = if tbRunning then "thunderbird-on.png" else "thunderbird-off.png"
  return $ dir ++ "/" ++ img

unreadCountsMarkup color = do
  home <- getHome
  let cmd = ["find", home ++ "/" ++ dir ++ "/", "-iname", "*.default"]
  profileDir <- fmap chompAll $ readProc cmd
  let ucFile = profileDir ++ "/unread-counts"

  unreadCounts <- chompFile ucFile
  let markup = formatUnreadCounts $ parseUnreadCounts unreadCounts
  return $ fg color markup


parseUnreadCounts :: String -> [(String, Integer)]
parseUnreadCounts uc = catMaybes $ map ucMatch $ lines uc

ucMatch :: String -> Maybe (String, Integer)
ucMatch s = fmap (\[count, name] -> (name, read count)) groups
  where regex = "^(\\d+):(.*)"
        groups = regexGroups regex s

formatUnreadCounts :: [(String, Integer)] -> String
formatUnreadCounts unreadCounts = (padL ' ' 3 top) ++ "\n" ++ (padL ' ' 3 bot)
  where uc = filter isDisplayable unreadCounts
        isDisplayable (name, count) = M.member name accounts && count > 0
        len = length uc
        counts = map snd uc
        max = maximum (0:counts)
        total = sum counts
        fmtIndex i = if i >= len then "   " else fmt $ uc !! i
        fmt (name, cnt) = show cnt ++ (fromMaybe "?" $ M.lookup name accounts)
        (top, bot) = if max > 99 || len > 2 then
                       (if total > 99 then "99+" else show total, "???")
                     else
                       (fmtIndex 0, fmtIndex 1)
