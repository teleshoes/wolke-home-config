module Net(main) where
import System.Process(readProcess)
import System.Environment (getEnv)
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Regex.PCRE
import TextRows (textRows)
import ClickAction (clickAction)

height = 36

cmd home = wscanCmd ++ " | " ++ popupCmd ++ dzenArgs
  where wscanCmd = home ++ "/.dzen2/printers/wscan"
        popupCmd = home ++ "/.dzen2/launchers/popup"
        dzenArgs = " 500 24 -fn inconsolata-14 "

main = do
  home <- getEnv "HOME"
  wlan <- chomp <$> readProcess "wlan" [] ""
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
  putStrLn $ clickAction "1" (cmd home) (textRows top bot height)

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

--my $mbps = $1 if $iwconfig =~ /Bit Rate=(\d+) Mb\/s/;

getMatch s p = listToMaybe $ concat $ map tail groupSets
  where groupSets = s =~ p :: [[String]]

chomp "" = ""
chomp s | last s == '\n' = reverse $ tail $ reverse s
chomp s | otherwise = s
