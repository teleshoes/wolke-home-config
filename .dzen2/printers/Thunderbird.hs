module Thunderbird(main) where
import Utils (
  fg, bg,
  shiftMid,
  chompAll, padL, isRunning)
import TextRows (textRows)
import ClickAction (clickActions)

import qualified Data.Map as M (fromList, lookup, member)

import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import System.Process (readProcessWithExitCode)
import Text.Regex.PCRE ((=~))

clickCommands = [ ""
                  ++ "pidof thunderbird-bin; "
                  ++ "if [ $? == 0 ]; then "
                    ++ "xdotool key --clearmodifiers alt+8; "
                  ++ "else "
                    ++ "thunderbird; "
                  ++ "fi"
                , "thunderbird --compose"
                , "killall thunderbird-bin"
                ]

accounts = M.fromList [ ("Gmail", "G")
                      , ("LilleGroup", "L")
                      , ("AOL - LiberiFataliVIII", "A")
                      ]

getProfileDir = do
  home <- getEnv "HOME"
  let cmd = ["find", home ++ "/.thunderbird/", "-iname", "*.default"]
  (_, out, _) <- readProcessWithExitCode (head cmd) (tail cmd) ""
  return out

main = do
  home <- getEnv "HOME"
  let imgSize = 16
  let imgSubDir = show imgSize ++ "x" ++ show imgSize
  let imgPath = home ++ "/.dzen2/icons/" ++ imgSubDir ++ "/thunderbird.xpm"

  profileDir <- fmap chompAll getProfileDir
  let ucFile = profileDir ++ "/unread-counts"

  tbRunning <- isRunning "thunderbird-bin"
  let runningMarkup = if tbRunning then " " else fg "red" "x"

  unreadCountsExists <- doesFileExist ucFile
  unreadCounts <- if unreadCountsExists then readFile ucFile else return ""
  let unreadMarkup = formatUnreadCounts $ parseUnreadCounts unreadCounts

  putStrLn $
    clickActions clickCommands $
    bg "black" $ shiftMid ++ runningMarkup ++ fg "green" unreadMarkup

parseUnreadCounts :: String -> [(String, Integer)]
parseUnreadCounts uc = catMaybes $ map ucMatch $ lines uc

ucMatch :: String -> Maybe (String, Integer)
ucMatch s = if isMatch && okAccount then Just (display, count) else Nothing
  where regex = "^(\\d+):(.*)"
        match = s =~ regex :: [[String]]
        isMatch = length match == 1
        count = read $ head match !! 1
        name = head match !! 2
        okAccount = M.member name accounts && count > 0
        display = fromMaybe "" $ M.lookup name accounts

formatUnreadCounts :: [(String, Integer)] -> String
formatUnreadCounts uc = textRows (padL ' ' 3 top) (padL ' ' 3 bot)
  where len = length uc
        counts = map snd uc
        max = maximum (0:counts)
        total = sum counts
        fmtIndex i = if i >= len then "   " else fmt $ uc !! i
        fmt (disp, cnt) = show cnt ++ disp
        (top, bot) = if max > 99 || len > 2 then
                       (if total > 99 then "99+" else show total, "???")
                     else
                       (fmtIndex 0, fmtIndex 1)
