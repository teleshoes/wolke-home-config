import qualified Widgets as W
import Color (Color(..), hexColor)
import WMLog (WMLogConfig(..))

import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, widgetSpacing, startWidgets, endWidgets)

main = do
  let cfg = defaultTaffybarConfig {barHeight=38, widgetSpacing=5}
      font = "Inconsolata medium 13"
      bgColor = hexColor $ RGB (0.247, 0.235, 0.427)
      fgColor = hexColor $ RGB (0.659, 0.682, 0.969)
      textColor = hexColor $ Black
      sep = W.sepW Black 2

      start = [ W.wmLogNew WMLogConfig { titleLength = 30
                                       , wsImageHeight = 24
                                       , titleRows = True
                                       , stackWsTitle = False
                                       , wsBorderColor =
                                           RGB (0.61, 0.458, 0.153)
                                       }
              ]
      end = reverse
          [ W.monitorCpuW
          , W.monitorMemW
          , W.netStatsW
          , sep
          , W.netW
          , sep
          , W.fcrondynW
          , sep
          , W.klompW
          , W.volumeW
          , W.micW
          , W.pidginPipeW $ barHeight cfg
          , W.thunderbirdW (barHeight cfg) Green Black
          , W.ekigaW
          , W.cpuScalingW
          , W.cpuFreqsW
          , W.fanW
          , W.brightnessW
          , W.pingMonitorW "www.google.com" "G"
          , W.openvpnW
          , W.pingMonitorW "source.escribe.com" "E"
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
