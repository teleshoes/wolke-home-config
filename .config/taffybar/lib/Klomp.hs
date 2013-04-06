module Klomp(klompW) where
import Widgets (clickable)
import Utils (padL, padR, isRunning, chompFile, readProc)

import TextRows (textRows)

import Prelude hiding(lookup)
import Data.Map (fromList, Map, lookup)
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Regex.PCRE ((=~))

rowLength = 34
gapOffset = 3
sep = "…"

clickL = Just "xdotool key --clearmodifiers alt+9; klomp-term"
clickM = Just "klomp-cmd reset"
clickR = Just "klomp-cmd stop"

strLookup :: Ord a => a -> Map a String -> String
strLookup k m = fromMaybe "" $ lookup k m

getRemoteCur = chompFile "/tmp/klomp-dzen"

readKlompCur remoteCur = case remoteCur of
                           "n9" -> readProc ["n9u", "-b", "cat ~/.klompcur"]
                           "raspi" -> readProc ["pi", "-b", "cat ~/.klompcur"]
                           _ -> do home <- getEnv "HOME"
                                   chompFile $ home ++ "/" ++ ".klompcur"

getMarkup = do
  remoteCur <- getRemoteCur
  let isRemote = not $ null remoteCur
  cur <- readKlompCur remoteCur
  running <- isRunning "klomplayer"

  let ((posSex, lenSex, path), atts) = parseCur cur
  let [pos,len] = formatTimes [posSex, lenSex]

  let prefix = if isRemote then "%" else if running then "" else "x"
  let (top, bot) = if length cur > 0 then
                      ( pos ++ "-" ++ strLookup "artist" atts
                      , len ++ "-" ++ strLookup "title" atts
                      )
                   else
                      ( "              KLOMP      "
                      , "          no current song"
                      )
  return $ textRows (adjustLen $ prefix ++ top) (adjustLen $ prefix ++ bot)

klompW w = do
  lbl <- w getMarkup
  click <- clickable lbl clickL clickM clickR
  return click

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
        sepLen = length sep
        gapStart = (rowLength `div` 2) - (sepLen `div` 2) + gapOffset
        gapLength = strLen - rowLength + sepLen
        beforeGap = take (gapStart-1) s
        afterGap = drop (gapStart - 1 + gapLength) s

