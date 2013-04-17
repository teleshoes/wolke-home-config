import qualified Widgets as W
import Color (Color(..))
import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, widgetSpacing, startWidgets, endWidgets)

main = do
  let cfg = defaultTaffybarConfig {barHeight=38, widgetSpacing=5}
      font = "Inconsolata medium 13"
      bgColor = "#333366"
      fgColor = "#9999ff"
      textColor = "#FFFFFF"
      sep = W.sepW Black 2

      titleLength = 30
      wsImageHeight = 24
      titleRows = True
      stackWsTitle = False

      start = [ W.wmLogNew titleLength wsImageHeight titleRows stackWsTitle ]
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
