module Net(main) where
import System.IO (stdout, hFlush)
import System.Process(readProcess, system)
import System.Posix.Process (forkProcess)
import System.Posix.IO (stdInput, stdOutput, stdError, closeFd)
import System.Environment (getEnv)
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (void, forever)
import Control.Monad.Loops (anyM)
import Data.Maybe (listToMaybe)
import Text.Regex.PCRE
import TextRows (textRows)
import ClickAction (clickAction)

height = 36

cmd home = wscanCmd ++ " | " ++ popupCmd ++ dzenArgs
  where wscanCmd = home ++ "/.dzen2/printers/wscan"
        popupCmd = home ++ "/.dzen2/launchers/popup"
        dzenArgs = " 500 24 -fn inconsolata-14 "

data WStatus = Wlan | Wired | PPP | Tethering | None | Unknown deriving(Eq)

readWStatus :: IO WStatus
readWStatus = do 
  wstatus <- readProcess "wstatus" [] ""
  case wstatus of
    "wlan\n"       -> return Wlan
    "eth\n"        -> return Wired
    "ppp\n"        -> return PPP
    "tethering\n"  -> return Tethering
    "none\n"       -> return None
    otherwise      -> return Unknown


main = forever $ do
  wstatus <- readWStatus
  case wstatus of
    Wlan      -> printNet =<< wifi
    Wired     -> printNet "wired"
    PPP       -> printNet "pewpewpew"
    Tethering -> printNet "tethering"
    None      -> printNet "no wabs"
    Unknown   -> printNet "???"
  hFlush stdout
  threadDelay $ 1*10^6

printNet text = do
  home <- getEnv "HOME"
  putStrLn $ clickAction "1" (cmd home) text

wifi = do
  wlan <- chomp <$> readProcess "ifdev" ["wlan"] ""
  s <- readProcess "iwconfig" [wlan] ""
  let ssid = getMatch s "ESSID:\"(.*)\""
  let freq = getMatch s "Frequency:(\\d+(\\.\\d+)?) GHz"
  let qTop = getMatch s "Link Quality=(\\d+)/\\d+"
  let qBot = getMatch s "Link Quality=\\d+/(\\d+)"
  let rate = getMatch s "Bit Rate=(\\d+) Mb/s"
  let q = quality qTop qBot
  let f = frequency freq
  let top = (padtrim 3 rate ++ "m") ++ "|" ++ (quality qTop qBot)
  let bot = (padtrim 9 ssid)
  return $ textRows top bot height

i = read :: String -> Integer
d = read :: String -> Double

padtrim len (Just s) = pad len $ take len $ s
padtrim len Nothing = take len $ repeat '?'

frequency (Just f) = show $ round $ (1000.0 * (d f))
frequency Nothing = "????"

quality (Just top) (Just bot) = pad 4 (per ++ "%")
  where per = show $ 100 * (i top) `div` (i bot)
quality _ _ = "???%"

pad len s | length s < len = pad len (' ':s)
pad _ s = s

getMatch s p = listToMaybe $ concat $ map tail groupSets
  where groupSets = s =~ p :: [[String]]

chomp "" = ""
chomp s | last s == '\n' = reverse $ tail $ reverse s
chomp s | otherwise = s
