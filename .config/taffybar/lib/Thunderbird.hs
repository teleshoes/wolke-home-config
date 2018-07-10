module Thunderbird(thunderbirdW) where
import Clickable (clickable)
import Image (imageW)
import Label (labelW, mainLabel)
import Utils (
  eboxStyleWrapW, imageDir, fg, getHome, regexGroups, chompAll, padL,
  isRunning, readProc, chompFile)

import GI.Gtk.Enums (
  Orientation(OrientationHorizontal))
import GI.Gtk.Objects.Box (boxNew, boxSetHomogeneous)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Widget (Widget, toWidget)
import System.Taffybar.Widget.Util (widgetSetClassGI)

import qualified Data.Map as M (fromList, lookup, member)

import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getEnv)

main = mainLabel $ unreadCountsMarkup
thunderbirdW h = do
  img <- imageW (getImage h)
  label <- labelW $ unreadCountsMarkup

  box <- boxNew OrientationHorizontal 0
  boxSetHomogeneous box False

  containerAdd box img
  containerAdd box label

  box <- clickable clickL clickM clickR box
  eboxStyleWrapW box "Email"


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

unreadCountsMarkup = do
  home <- getHome
  let cmd = ["find", home ++ "/" ++ dir ++ "/", "-iname", "*.default"]
  profileDir <- fmap chompAll $ readProc cmd
  let ucFile = profileDir ++ "/unread-counts"

  unreadCounts <- chompFile ucFile
  let markup = formatUnreadCounts $ parseUnreadCounts unreadCounts
  return markup


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
