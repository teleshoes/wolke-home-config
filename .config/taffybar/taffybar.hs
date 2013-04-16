import qualified Widgets as W
import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, widgetSpacing, startWidgets, endWidgets)

main = do
  let cfg = defaultTaffybarConfig {barHeight=38, widgetSpacing=5}
      font = "Inconsolata medium 13"
      bgColor = "#3f3c6d"
      fgColor = "#a8aef7"
      textColor = "#FFFFFF"

      titleLength = 30
      wsImageHeight = 24
      titleRows = True
      stackWsTitle = False

      start = [ W.wmLogNew titleLength wsImageHeight titleRows stackWsTitle ]
      end = reverse
          [ W.monitorCpuW
          , W.monitorMemW
          , W.netStatsW
          , W.sepW
          , W.netW
          , W.sepW
          , W.fcrondynW
          , W.sepW
          , W.klompW
          , W.volumeW
          , W.micW
          , W.pidginPipeW $ barHeight cfg
          , W.thunderbirdW $ barHeight cfg
          , W.ekigaW
          , W.cpuScalingW
          , W.cpuFreqsW
          , W.fanW
          , W.brightnessW
          , W.pingMonitorW "www.google.com" "G"
          , W.openvpnW
          , W.pingMonitorW "source.escribe.com" "E"
          , W.tpBattStatW $ barHeight cfg
          , W.sepW
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
