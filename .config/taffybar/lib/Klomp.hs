{-# LANGUAGE DeriveGeneric #-}
module Klomp(klompW, main) where
import Clickable (clickable)
import Label (labelW, mainLabel)
import Utils (
  decodeSingleRowCsv,
  regexMatch, stringWidth, trimL, trimR, padL, padR,
  fgbg, escapeMarkup,
  isRunning, chompFile, readProc, readInt, readDouble)

import Data.Csv (DefaultOrdered, FromRecord, FromNamedRecord, ToNamedRecord)
import Data.Either (fromLeft, fromRight)
import GHC.Generics (Generic)
import System.Taffybar.Widget.Util (widgetSetClassGI)
import Codec.Binary.UTF8.String (utf8Encode, decodeString)
import qualified Data.Text as T (pack)

main = mainLabel $ klompReader 30

klompW rowLength = do
  label <- labelW (klompReader rowLength)
  widgetSetClassGI label $ T.pack "Klomp"
  clickable clickL clickM clickR label

gapOffset = 3
sep = "…"
sxPrefix = "ŝ"
raspiPrefix = "ř"
nucPrefix = "₪"

clickL = Just "klomp-term --playlist"
clickM = Just "klomp-cmd reset"
clickR = Just "klomp-cmd stop"

klompInfoCmd = ["klomp-info", "-s", "-c"] ++ infoColumns
  where infoColumns = [ "title", "artist", "album", "number"
                      , "pos", "len", "percent"
                      , "playlist", "ended"
                      ]

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
  let utf8Str = utf8Encode str
  return $ parseKlompInfo utf8Str


data KlompInfo = KlompInfo { title    :: !String
                           , artist   :: !String
                           , album    :: !String
                           , number   :: !String
                           , pos      :: !Double
                           , len      :: !Double
                           , percent  :: !Integer
                           , playlist :: !String
                           , ended    :: !String
                           } deriving (Generic, Show)
instance FromRecord KlompInfo
instance ToNamedRecord KlompInfo
instance FromNamedRecord KlompInfo
instance DefaultOrdered KlompInfo

emptyKlompInfo = KlompInfo { title = "", artist = "", album = "", number = ""
                           , pos = 0.0, len = 0.0, percent = 0
                           , playlist = "", ended = ""
                           }

parseKlompInfo :: String -> Either String KlompInfo
parseKlompInfo klompInfoStr | noSongFound = Left "(no song info found)"
                            | otherwise   = res
  where noSongFound = regexMatch (decodeString klompInfoStr) "^No Song info found"
        res = case csvRes of
                Left csvParseErr -> Left $ "(ERROR: klomp-info " ++ csvParseErr ++ ")"
                Right klompInfo  -> Right klompInfo
        csvRes = decodeSingleRowCsv klompInfoStr :: Either String KlompInfo

formatKlompInfo :: (Either String KlompInfo) -> Int -> Maybe String -> Bool -> String
formatKlompInfo klompInfoRes rowLength ipmagicName klompRunning = topLine ++ "\n" ++ botLine
  where [posFmt, lenFmt] = formatTimes $ map round [pos klompInfo, len klompInfo]
        klompInfo = fromRight emptyKlompInfo klompInfoRes
        endFmt = if null $ ended klompInfo then "" else "*ENDED*"
        errFmt = fromLeft "" klompInfoRes
        artFmt = artist klompInfo
        titFmt = title klompInfo
        topPrefix = case ipmagicName of
                      Just "sx" -> sxPrefix
                      Just "raspi" -> raspiPrefix
                      Just "nuc" -> nucPrefix
                      _ -> if klompRunning then "" else "x"
        botPrefix = take 1 $ playlist klompInfo

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
