module Thunderbird(main) where
import Utils (
  fg, bg, img,
  posX, shiftTop, shiftBot,
  regexGroups, chompAll, padL, isRunning, readProc, chompFile)
import TextRows (textRows)
import ClickAction (clickActions)

import qualified Data.Map as M (fromList, lookup, member)

import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getEnv)

exec = "icedove"
process = exec ++ "-bin"
dir = "." ++ exec

clickCommands = [ ""
                  ++ "pidof " ++ process ++ "; "
                  ++ "if [ $? == 0 ]; then "
                    ++ "xdotool key --clearmodifiers alt+8; "
                  ++ "else "
                    ++ exec ++ "; "
                  ++ "fi"
                , exec ++ " --compose"
                , "killall " ++ process
                ]

accounts = M.fromList [ ("Gmail", "G")
                      , ("LilleGroup", "L")
                      , ("AOL - LiberiFataliVIII", "A")
                      , ("teleshoes", "T")
                      ]

main = do
  home <- getEnv "HOME"
  let imgSize = 16
  let imgSubDir = show imgSize ++ "x" ++ show imgSize
  let imgPath = home ++ "/.dzen2/icons/" ++ imgSubDir ++ "/thunderbird.xpm"

  let cmd = ["find", home ++ "." ++ dir ++ "/", "-iname", "*.default"]
  profileDir <- fmap chompAll $ readProc cmd
  let ucFile = profileDir ++ "/unread-counts"

  tbRunning <- isRunning process
  let runningMarkup = if tbRunning then "  " else fg "red" " X"

  unreadCounts <- chompFile ucFile
  let unreadMarkup = formatUnreadCounts $ parseUnreadCounts unreadCounts

  let icon = posX (-imgSize) ++ shiftTop ++ img imgPath

  putStrLn $
    clickActions clickCommands $
    bg "black" $ shiftBot ++ runningMarkup ++ icon ++ fg "green" unreadMarkup

parseUnreadCounts :: String -> [(String, Integer)]
parseUnreadCounts uc = catMaybes $ map ucMatch $ lines uc

ucMatch :: String -> Maybe (String, Integer)
ucMatch s = fmap (\[count, name] -> (name, read count)) groups
  where regex = "^(\\d+):(.*)"
        groups = regexGroups regex s

formatUnreadCounts :: [(String, Integer)] -> String
formatUnreadCounts unreadCounts = textRows (padL ' ' 3 top) (padL ' ' 3 bot)
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
