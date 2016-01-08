import qualified Widgets as W
import Color (Color(..), hexColor)
import WMLog (WMLogConfig(..))
import Utils (colW)

import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, barPosition, widgetSpacing, startWidgets, endWidgets,
  Position(Top, Bottom))

import Data.Functor ((<$>))
import System.Environment (getArgs)

main = do
  isBot <- elem "--bottom" <$> getArgs
  let cfg = defaultTaffybarConfig { barHeight=38
                                  , widgetSpacing=4
                                  , barPosition=if isBot then Bottom else Top
                                  }
      font = "Inconsolata medium 12"
      fgColor = hexColor $ RGB (0x93/0xff, 0xa1/0xff, 0xa1/0xff)
      bgColor = hexColor $ RGB (0x00/0xff, 0x2b/0xff, 0x36/0xff)
      textColor = hexColor $ Black
      sep = W.sepW Black 2

      start = [ W.wmLogNew WMLogConfig
                { titleLength = 30
                , wsImageHeight = 16
                , titleRows = True
                , stackWsTitle = False
                , wsBorderColor = RGB (0.6, 0.5, 0.2)
                }
              ]
      end = reverse
          [ W.monitorCpuW 30
          , W.monitorMemW 30
          , W.progressBarW
          , W.netStatsW
          , sep
          , W.netW
          , sep
          , W.fcrondynW
          , sep
          , W.widthScreenWrapW 0.159375 =<< W.klompW
          , W.volumeW
          , W.micW
          , W.pidginPipeW $ barHeight cfg
          , W.qtemailW (barHeight cfg) Green Black
          , W.ekigaW
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

  rcParseString $ ""
        ++ "style \"default\" {"
        ++ "  font_name = \"" ++ font ++ "\""
        ++ "  bg[NORMAL] = \"" ++ bgColor ++ "\""
        ++ "  fg[NORMAL] = \"" ++ fgColor ++ "\""
        ++ "  text[NORMAL] = \"" ++ textColor ++ "\""
        ++ "}"
  defaultTaffybar cfg {startWidgets=start, endWidgets=end}
