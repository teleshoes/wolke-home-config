module Klomp(main) where
import Utils (isRunning)
import ClickAction (clickActionSet)
import TextRows (textRows)

import Prelude hiding(lookup)
import Data.Map (fromList, Map, lookup)
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Regex.PCRE ((=~))

rowLength = 34
gapOffset = 3

btn1Cmd = "xdotool key --clearmodifiers alt+9; klomp-term"
btn2Cmd = "klomp-cmd reset"
btn3Cmd = "klomp-cmd stop"

strLookup :: Ord a => a -> Map a String -> String
strLookup k m = fromMaybe "" $ lookup k m

main = do
  home <- getEnv "HOME"
  let curFile = home ++ "/" ++ ".klompcur"
  curExists <- doesFileExist curFile
  cur <- if curExists then readFile curFile else return ""
  running <- isRunning "klomplayer"

  let ((posSex, lenSex, path), atts) = parseCur cur
  let [pos,len] = formatTimes [posSex, lenSex]

  let prefix = if running then "" else "x" 
  let (top, bot) = if curExists then
                      ( pos ++ "-" ++ strLookup "artist" atts
                      , len ++ "-" ++ strLookup "title" atts
                      )
                   else
                      ( "              KLOMP      "
                      , "          no current song"
                      )

  putStr
    $ clickActionSet btn1Cmd btn2Cmd btn3Cmd
    $ textRows (adjustLen $ prefix ++ top) (adjustLen $ prefix ++ bot)

toFloat = read :: String -> Float

parseCur :: String -> ((Integer, Integer, String), Map String String)
parseCur cur = (infoMatch info, fromList $ catMaybes $ map attMatch atts)
  where lns = lines cur
        (info:atts) = if length lns > 0 then lns else [""]

infoMatch :: String -> (Integer, Integer, String)
infoMatch s = if isMatch then (posSex, lenSex, path) else (0, 0, "")
  where regex = "(\\d+(?:\\.\\d+)?) (\\d+(?:\\.\\d+)?) (.*)"
        match = s =~ regex :: [[String]]
        isMatch = length match == 1
        posSex = round $ toFloat $ head match !! 1
        lenSex = round $ toFloat $ head match !! 2
        path = head match !! 3

attMatch :: String -> Maybe (String, String)
attMatch s = if isMatch then Just (key, val) else Nothing
  where regex = "([a-z_A-Z]+)=(.*)"
        match = s =~ regex :: [[String]]
        isMatch = length match == 1
        key = head match !! 1
        val = head match !! 2

formatTimes ts = map fmt ts 
  where maxH = (maximum ts) `div` (60^2)
        maxHLen = length $ show $ maxH
        fmt t = (if maxH > 0 then h t ++ ":" else "") ++ m t ++ ":" ++ s t
        h t = padL '0' maxHLen $ show $ t `div` 60^2
        m t = padL '0' 2 $ show $ (t `mod` 60^2) `div` 60
        s t = padL '0' 2 $ show $ t `mod` 60

adjustLen s = padR ' ' rowLength $ sTrim
  where strLen = length s
        sTrim = if strLen > rowLength then beforeGap ++ sep ++ afterGap else s
        sep = "…"
        sepLen = length sep
        gapStart = (rowLength `div` 2) - (sepLen `div` 2) + gapOffset
        gapLength = strLen - rowLength + sepLen
        beforeGap = take (gapStart-1) s
        afterGap = drop (gapStart - 1 + gapLength) s

padR x len xs = xs ++ replicate (len - length xs) x
padL x len xs = replicate (len - length xs) x ++ xs
