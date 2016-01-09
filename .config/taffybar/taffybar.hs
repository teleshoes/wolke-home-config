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
  let cfg = defaultTaffybarConfig { barHeight=42
                                  , widgetSpacing=5
                                  , barPosition=if isBot then Bottom else Top
                                  }

      font = "Inconsolata medium 16"
      fgColor = hexColor $ RGB (0x93/0xff, 0xa1/0xff, 0xa1/0xff)
      bgColor = hexColor $ RGB (0x00/0xff, 0x2b/0xff, 0x36/0xff)
      textColor = hexColor $ Black
      sep = W.sepW Black 3

      start = [ W.wmLogNew WMLogConfig
                { titleLength = 30
                , wsImageHeight = 28
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
          , W.widthScreenWrapW 0.194271 =<< W.klompW
          , sep
          , W.speakerW
          , W.volumeW
          , W.micW
          , W.cpuIntelPstateW
          , W.cpuFreqsW
          , W.brightnessW
          , W.screenSaverW
          , colW [ W.pingMonitorW "G" "www.google.com"
                 ]
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
