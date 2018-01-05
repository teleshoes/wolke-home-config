import qualified Widgets as W
import Color (Color(..), hexColor)
import WMLog (WMLogConfig(..))
import Utils (colW, attemptCreateSymlink)
import Width (charsFitInPx, getScreenDPI, screenPctToPx)

import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, barPosition, widgetSpacing, startWidgets, endWidgets,
  Position(Top, Bottom))

import Data.Functor ((<$>))
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir ( getUserConfigFile )

profile = profileT430s

--                 pName  barHt  wImgH  space  wSepW  title  fontP  graph  music
profileT430s = P "t430s"     38     24      5      2     30   13.0     50  15.94
profileW520  = P  "w520"     38     24      5      2     30   13.0     50  15.63
profileNuc   = P   "nuc"     42     28      5      3     30   16.0     50  19.43

typeface = "Inconsolata"

data Profile = P { pName :: String --profile name
                 , barHt :: Int    --bar height in pixels
                 , wImgH :: Int    --workspace image height in pixels
                 , space :: Int    --widget spacing in pixels
                 , wSepW :: Int    --widget separator width in pixels
                 , title :: Int    --window title length in characters
                 , fontP :: Double --font point size
                 , graph :: Int    --width of graphs in pixels
                 , music :: Double --percent of the screen width to use for song info
                 }

main = do
  dpi <- getScreenDPI
  isBot <- elem "--bottom" <$> getArgs
  klompWidthPx <- screenPctToPx $ music profile
  let cfg = defaultTaffybarConfig { barHeight = barHt profile
                                  , widgetSpacing = space profile
                                  , barPosition = if isBot then Bottom else Top
                                  }
      bgColor = hexColor $ RGB (0x00/0xff, 0x2b/0xff, 0x36/0xff)
      textColor = hexColor $ RGB (0x93/0xff, 0xa1/0xff, 0xa1/0xff)
      wsBorderColorNormal = hexColor $ RGB (0xD4/0xff, 0xAD/0xff, 0x35/0xff)
      wsBorderColorActive = hexColor Red
      sep = W.sepW Black $ wSepW profile
      klompChars = charsFitInPx dpi (fontP profile) klompWidthPx

      start = [ W.wmLogNew WMLogConfig
                { titleLength = title profile
                , wsImageHeight = wImgH profile
                , titleRows = True
                , stackWsTitle = False
                }
              ]
      end = reverse
          [ W.monitorCpuW $ graph profile
          , W.monitorMemW $ graph profile
          , W.syncWatchW
          , W.progressBarW
          , W.netStatsW
          , sep
          , W.netW
          , sep
          , W.fcrondynW
          , sep
          , (W.widthCharWrapW dpi (fontP profile) klompChars) =<< W.klompW klompChars
          , W.volumeW
          , W.micW
          , W.pidginPipeW $ barHeight cfg
          , W.qtemailW (barHeight cfg) Green Black
          , W.cpuIntelPstateW
          , W.cpuFreqsW
          , W.fanW
          , W.brightnessW
          , colW [ W.pingMonitorW "G" "www.google.com"
                 , W.pingMonitorW "E" "ehr.dev"
                 ]
          , W.openvpnW "aws" "VPN\naws"
          , W.tpBattStatW $ barHeight cfg
          , sep
          , W.clockW
          ]

  cssProfileFile <- getUserConfigFile "taffybar" "taffybar-profile.css"

  writeFile cssProfileFile $ ""
        ++ "/* profile: " ++ pName profile ++ "\n"
        ++ " * auto-generated at: " ++ cssProfileFile ++ "\n"
        ++ " * taffybar-height: " ++ (show $ barHt profile) ++ "\n"
        ++ " */\n"
        ++ "@define-color bgColor " ++ bgColor ++ ";\n"
        ++ "@define-color textColor " ++ textColor ++ ";\n"
        ++ "@define-color wsBorderColorNormal " ++ wsBorderColorNormal ++ ";\n"
        ++ "@define-color wsBorderColorActive " ++ wsBorderColorActive ++ ";\n"
        ++ "label {\n"
        ++ "  font: " ++ (show $ fontP profile) ++ "pt " ++ show typeface ++ ";\n"
        ++ "}\n"

  defaultTaffybar cfg {startWidgets=start, endWidgets=end}
