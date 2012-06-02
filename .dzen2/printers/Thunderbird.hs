module Thunderbird(main) where
import Utils (
  fg, bg, img,
  posX, shiftTop, shiftBot,
  chompAll, padL, isRunning, readProc, chompFile)
import TextRows (textRows)
import ClickAction (clickActions)

import qualified Data.Map as M (fromList, lookup, member)

import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getEnv)
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

main = do
  home <- getEnv "HOME"
  let imgSize = 16
  let imgSubDir = show imgSize ++ "x" ++ show imgSize
  let imgPath = home ++ "/.dzen2/icons/" ++ imgSubDir ++ "/thunderbird.xpm"

  let cmd = ["find", home ++ "/.thunderbird/", "-iname", "*.default"]
  profileDir <- fmap chompAll $ readProc cmd
  let ucFile = profileDir ++ "/unread-counts"

  tbRunning <- isRunning "thunderbird-bin"
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
