{-# LANGUAGE OverloadedStrings #-}
module Klomp(klompW, main) where
import Clickable (clickable)
import Label (labelW, mainLabel)
import Utils (
  regexMatch, stringWidth, trimL, trimR, padL, padR,
  fgbg, isRunning, chompFile, readProc)

import System.Taffybar.Widget.Util (widgetSetClassGI)
import Codec.Binary.UTF8.String (utf8Encode, decodeString)
import Control.Applicative ((<$>), (<*>))
import Data.Csv (decodeByName, FromNamedRecord, parseNamedRecord, (.:))
import GI.GLib.Functions (markupEscapeText)
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

getKlompInfo ipmagicName = do
  str <- readProc $ cmd ipmagicName
  return $ case decodeByName (encodeUtf8 $ T.pack $ coerceUtf8 str) of
    Left msg -> emptyKlompInfo {errorMsg = formatErr str msg}
    Right (hdr, csv) -> if Vector.length csv /= 1
                        then emptyKlompInfo {errorMsg = "rowcount != 1"}
                        else Vector.head csv
  where cmd (Just ipmagicName) = ["ipmagic", ipmagicName] ++ klompInfoCmd
        cmd Nothing = klompInfoCmd
        formatErr klompInfo parseError = if regexMatch "No song info found" klompInfo
                                         then "(no song info found)"
                                         else parseError

coerceUtf8 = decodeString . utf8Encode

klompReader rowLength = do
  klompBarIpmagic <- chompFile "/tmp/klomp-bar-ipmagic"
  let ipmagicName = if klompBarIpmagic == "" then Nothing else Just klompBarIpmagic
  running <- isRunning "klomplayer"
  klompInfo <- getKlompInfo ipmagicName
  let errFmt = errorMsg klompInfo
      titFmt = T.unpack $ title klompInfo
      artFmt = T.unpack $ artist klompInfo
      albFmt = T.unpack $ artist klompInfo
      numFmt = T.unpack $ artist klompInfo
      [posFmt, lenFmt] = formatTimes $ map round [pos klompInfo, len klompInfo]
      perFmt = percent klompInfo
      plsFmt = T.unpack $ playlist klompInfo
      endFmt = if null $ T.unpack $ ended klompInfo then "" else "*ENDED*"

  let topPrefix = case ipmagicName of
                    Just "sx" -> sxPrefix
                    Just "raspi" -> raspiPrefix
                    Just "nuc" -> nucPrefix
                    _ -> if running then "" else "x"
      botPrefix = take 1 plsFmt

  let isPrefixed = not((null topPrefix) && (null botPrefix))
      topPrefixFmt = if isPrefixed then prefixFmt topPrefix else ""
      botPrefixFmt = if isPrefixed then prefixFmt botPrefix else ""
      rowLen = if isPrefixed then rowLength - 1 else rowLength

  let topText = padSquish rowLen $ posFmt ++ "-" ++ errFmt ++ artFmt
      botText = padSquish rowLen $ lenFmt ++ "-" ++ endFmt ++ titFmt

  top <- escapeMarkup topText
  bot <- escapeMarkup botText

  return $ topPrefixFmt ++ top ++ "\n" ++ botPrefixFmt ++ bot

escapeMarkup :: String -> IO String
escapeMarkup text = do
  markup <- markupEscapeText (pack text) (fromIntegral $ length text)
  return $ unpack markup

prefixFmt = fgbg "green" "black" . padSquish 1

infoColumns = ["title", "artist", "album", "number",
               "pos", "len", "percent", "playlist", "ended"]
klompInfoCmd = ["klomp-info", "-c", "-h", "-s"] ++ infoColumns

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


formatTimes ts = map fmt ts
  where maxH = (maximum ts) `div` (60^2)
        maxHLen = length $ show $ maxH
        fmt t = (if maxH > 0 then h t ++ ":" else "") ++ m t ++ ":" ++ s t
        h t = padL '0' maxHLen $ show $ t `div` 60^2
        m t = padL '0' 2 $ show $ (t `mod` 60^2) `div` 60
        s t = padL '0' 2 $ show $ t `mod` 60

padSquish len s = padR ' ' len $ sTrim
  where strLen = stringWidth s
        sTrim = if strLen > len then prefix ++ sep ++ suffix else s
        sepLen = stringWidth sep
        prefixLen = (len `div` 2) - (sepLen `div` 2) + gapOffset
        suffixLen = len - prefixLen - sepLen
        prefix = trimR prefixLen s
        suffix = trimL suffixLen s
