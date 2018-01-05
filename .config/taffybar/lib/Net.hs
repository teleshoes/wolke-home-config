module Net(netW) where
import Clickable (clickableLeft)
import Label (labelW, mainLabel)
import Utils (padL, chompAll, regexFirstGroup, chompFile)

import System.Process(readProcess)
import System.Environment (getEnv)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Maybe (listToMaybe)

main = mainLabel netReader
netW = clickableLeft wscanCmd =<< labelW netReader

lastSSIDFile = "/tmp/last-ssid"

width = 10

wscanCmd = "term 'echo LAST:; wscan -l; loop 1 \"echo; echo; date; wscan\"'"

data WStatus = Wlan | Wired | PPP |
               Wconnect | Wauto | Tethering |
               None | Unknown deriving(Eq)

readWStatus :: IO WStatus
readWStatus = do
  wstatus <- readProcess "wstatus" [] ""
  case wstatus of
    "wlan\n"       -> return Wlan
    "eth\n"        -> return Wired
    "ppp\n"        -> return PPP
    "wconnect\n"   -> return Wconnect
    "wauto\n"      -> return Wauto
    "tethering\n"  -> return Tethering
    "none\n"       -> return None
    otherwise      -> return Unknown

netReader = do
  wstatus <- readWStatus
  markup <- case wstatus of
    Wlan      -> wifi
    Wired     -> message "wired"
    PPP       -> message "pewpewpew"
    Wconnect  -> lastSSID
    Wauto     -> message "wauto"
    Tethering -> message "tethering"
    None      -> message "no wabs"
    Unknown   -> message "???"
  return markup

message s = return $ padtrim width $ Just s

lastSSID = do
  last <- chompFile lastSSIDFile
  top <- message "wconnect"
  bot <- message last
  return $ top ++ "\n" ++ bot

wifi = do
  wlan <- fmap chompAll $ readProcess "ifdev" ["wl", "wlan"] ""
  s <- readProcess "iwconfig" [wlan] ""
  let ssid = regexFirstGroup "ESSID:\"(.*)\"" s
  let freq = regexFirstGroup "Frequency:(\\d+(\\.\\d+)?) GHz" s
  let qTop = regexFirstGroup "Link Quality=(\\d+)/\\d+" s
  let qBot = regexFirstGroup "Link Quality=\\d+/(\\d+)" s
  let rate = regexFirstGroup "Bit Rate=(\\d+(?:\\.\\d+)?) Mb/s" s
  let r = rateFmt rate
  let q = quality qTop qBot
  let f = frequency freq
  let top = (padtrim (width-6) r ++ "m") ++ "|" ++ (quality qTop qBot)
  let bot = (padtrim width ssid)
  return $ top ++ "\n" ++ bot

i = read :: String -> Integer
d = read :: String -> Double

padtrim len (Just s) = padL ' ' len $ take len $ s
padtrim len Nothing = take len $ repeat '?'

rateFmt (Just r) = Just $ show $ round $ d r
rateFmt Nothing = Nothing

frequency (Just f) = show $ round $ (1000.0 * (d f))
frequency Nothing = "????"

quality (Just top) (Just bot) = padL ' ' 4 (per ++ "%")
  where per = show $ 100 * (i top) `div` (i bot)
quality _ _ = "???%"
