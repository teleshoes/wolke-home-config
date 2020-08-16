{-# LANGUAGE OverloadedStrings #-}
module Klomp(klompW, main) where
import Clickable (clickable)
import Label (labelW, mainLabel)
import Utils (
  regexMatch, stringWidth, trimL, trimR, padL, padR,
  fgbg, escapeMarkup,
  isRunning, chompFile, readProc)

import System.Taffybar.Widget.Util (widgetSetClassGI)
import Codec.Binary.UTF8.String (utf8Encode, decodeString)
import Control.Applicative ((<$>), (<*>))
import Data.Csv (decodeByName, FromNamedRecord, parseNamedRecord, (.:))
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy as T
import Data.Text (pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)

main = mainLabel $ klompReader 30
klompW rowLength = do
  label <- labelW (klompReader rowLength)
  widgetSetClassGI label $ pack "Klomp"
  clickable clickL clickM clickR label

gapOffset = 3
sep = "…"
sxPrefix = "ŝ"
raspiPrefix = "ř"
nucPrefix = "₪"

clickL = Just "klomp-term --playlist"
clickM = Just "klomp-cmd reset"
clickR = Just "klomp-cmd stop"

infoColumns = ["title", "artist", "album", "number",
               "pos", "len", "percent", "playlist", "ended"]
klompInfoCmd = ["klomp-info", "-c", "-h", "-s"] ++ infoColumns

getKlompInfoCmd :: Maybe String -> [String]
getKlompInfoCmd (Just ipmagicName) = "ipmagic":ipmagicName:klompInfoCmd
getKlompInfoCmd Nothing            = klompInfoCmd

klompReader rowLength = do
  klompBarIpmagic <- chompFile "/tmp/klomp-bar-ipmagic"
  let ipmagicName = if klompBarIpmagic == "" then Nothing else Just klompBarIpmagic
  klompRunning <- isRunning "klomplayer"
  klompInfo <- readKlompInfo ipmagicName

  return $ formatKlompInfo klompInfo rowLength ipmagicName klompRunning

readKlompInfo ipmagicName = do
  str <- readProc $ getKlompInfoCmd ipmagicName
  let utf8Str = decodeString $ utf8Encode str
  return $ case decodeByName (encodeUtf8 $ T.pack $ utf8Str) of
    Left msg -> emptyKlompInfo {errorMsg = formatErr utf8Str msg}
    Right (hdr, csv) -> getOnlyCsvRow csv
  where formatErr klompInfo parseError = if regexMatch "No song info found" klompInfo
                                         then "(no song info found)"
                                         else parseError
        getOnlyCsvRow csv = if Vector.length csv /= 1
                            then emptyKlompInfo {errorMsg = "rowcount != 1"}
                            else Vector.head csv


data KlompInfo = KlompInfo { errorMsg :: !String
                           , title    :: !T.Text
                           , artist   :: !T.Text
                           , album    :: !T.Text
                           , number   :: !T.Text
                           , pos      :: !Double
                           , len      :: !Double
                           , percent  :: !Int
                           , playlist :: !T.Text
                           , ended    :: !T.Text
                           } deriving Show
instance FromNamedRecord KlompInfo where
   parseNamedRecord m = KlompInfo <$> return "" <*>
                        m .: "title" <*>
                        m .: "artist" <*>
                        m .: "album" <*>
                        m .: "number" <*>
                        m .: "pos" <*>
                        m .: "len" <*>
                        m .: "percent" <*>
                        m .: "playlist" <*>
                        m .: "ended"

emptyKlompInfo = KlompInfo {errorMsg = "",
                            title = "", artist = "", album = "", number = "",
                            pos = 0.0, len = 0.0, percent = 0,
                            playlist = "", ended = ""}

formatKlompInfo :: KlompInfo -> Int -> Maybe String -> Bool -> String
formatKlompInfo klompInfo rowLength ipmagicName klompRunning = topLine ++ "\n" ++ botLine
  where [posFmt, lenFmt] = formatTimes $ map round [pos klompInfo, len klompInfo]
        endFmt = if null $ T.unpack $ ended klompInfo then "" else "*ENDED*"
        errFmt = errorMsg klompInfo
        artFmt = T.unpack $ artist klompInfo
        titFmt = T.unpack $ title klompInfo
        topPrefix = case ipmagicName of
                      Just "sx" -> sxPrefix
                      Just "raspi" -> raspiPrefix
                      Just "nuc" -> nucPrefix
                      _ -> if klompRunning then "" else "x"
        botPrefix = take 1 $ T.unpack $ playlist klompInfo

        isPrefixed = not((null topPrefix) && (null botPrefix))
        prefixFmt = fgbg "green" "black" . padSquish 1
        topPrefixFmt = if isPrefixed then prefixFmt topPrefix else ""
        botPrefixFmt = if isPrefixed then prefixFmt botPrefix else ""
        rowLen = if isPrefixed then rowLength - 1 else rowLength

        topText = padSquish rowLen $ posFmt ++ "-" ++ errFmt ++ artFmt
        botText = padSquish rowLen $ lenFmt ++ "-" ++ endFmt ++ titFmt

        topLine = topPrefixFmt ++ escapeMarkup topText
        botLine = botPrefixFmt ++ escapeMarkup botText

formatTimes :: [Int] -> [String]
formatTimes ts = map fmt ts
  where maxH = (maximum ts) `div` (60^2)
        maxHLen = length $ show $ maxH
        fmt t = (if maxH > 0 then h t ++ ":" else "") ++ m t ++ ":" ++ s t
        h t = padL '0' maxHLen $ show $ t `div` 60^2
        m t = padL '0' 2 $ show $ (t `mod` 60^2) `div` 60
        s t = padL '0' 2 $ show $ t `mod` 60

--pad to max len and elide the middle with ...
padSquish len s = padR ' ' len $ sTrim
  where strLen = stringWidth s
        sTrim = if strLen > len then prefix ++ sep ++ suffix else s
        sepLen = stringWidth sep
        prefixLen = (len `div` 2) - (sepLen `div` 2) + gapOffset
        suffixLen = len - prefixLen - sepLen
        prefix = trimR prefixLen s
        suffix = trimL suffixLen s
