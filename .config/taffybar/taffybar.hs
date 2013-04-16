import Widgets
import Graphics.UI.Gtk.General.RcStyle (rcParseString)
import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, widgetSpacing, startWidgets, endWidgets)

main = do
  let cfg = defaultTaffybarConfig {barHeight=40, widgetSpacing=5}
      font = "Inconsolata medium 13"
      bgColor = "#3f3c6d"
      fgColor = "#a8aef7"
      textColor = "#FFFFFF"

      titleLength = 30
      wsImageHeight = 24
      titleRows = True
      stackWsTitle = False

      start = [ wmLogNew titleLength wsImageHeight titleRows stackWsTitle ]
      end = reverse
          [ monitorCpuW
          , monitorMemW
          , netStatsW
          , sepW
          , netW
          , sepW
          , fcrondynW
          , sepW
          , klompW
          , volumeW
          , micW
          , pidginPipeW $ barHeight cfg
          , thunderbirdW $ barHeight cfg
          , ekigaW
          , cpuScalingW
          , cpuFreqsW
          , fanW
          , brightnessW
          , pingMonitorW "www.google.com" "G"
          , openvpnW
          , pingMonitorW "source.escribe.com" "E"
          , tpBattStatW $ barHeight cfg
          , sepW
          , clockW
          ]

  rcParseString $ ""
        ++ "style \"default\" {"
        ++ "  font_name = \"" ++ font ++ "\""
        ++ "  bg[NORMAL] = \"" ++ bgColor ++ "\""
        ++ "  fg[NORMAL] = \"" ++ fgColor ++ "\""
        ++ "  text[NORMAL] = \"" ++ textColor ++ "\""
        ++ "}"
  defaultTaffybar cfg {startWidgets=start, endWidgets=end}
