import qualified Widgets as W
import Color (Color(..), hexColor)
import WMLog (WMLogConfig(..))
import Utils (colW)
import Width (charsFitInPx, getScreenDPI, screenPctToPx)

import System.Taffybar (defaultTaffybar, defaultTaffybarConfig,
  barHeight, barPosition, widgetSpacing, startWidgets, endWidgets,
  Position(Top, Bottom))

import Data.Functor ((<$>))
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir ( getUserConfigFile )

profile = profileFHD

profileFHD = P { name = "fhd"
               , height = 42
               , spacing = 5
               , titleLen = 30
               , typeface = "Inconsolata medium"
               , fontSizePt = 16.0
               , graphWidth = 50
               , workspaceImageHeight = 28
               }
profileHDPlus = P { name = "hdplus"
                  , height = 38
                  , spacing = 4
                  , titleLen = 30
                  , typeface = "Inconsolata medium"
                  , fontSizePt = 12.0
                  , graphWidth = 30
                  , workspaceImageHeight = 16
                  }

main = do
  dpi <- getScreenDPI
  isBot <- elem "--bottom" <$> getArgs
  klompWidthPx <- screenPctToPx 19.4271
  let cfg = defaultTaffybarConfig { barHeight = height profile
                                  , widgetSpacing = spacing profile
                                  , barPosition = if isBot then Bottom else Top
                                  }

      font = (typeface profile) ++ " " ++ show (fontSizePt profile)
      fgColor = hexColor $ RGB (0x93/0xff, 0xa1/0xff, 0xa1/0xff)
      bgColor = hexColor $ RGB (0x00/0xff, 0x2b/0xff, 0x36/0xff)
      textColor = hexColor Black
      wsBorderColorNormal = hexColor $ RGB (0xD4/0xff, 0xAD/0xff, 0x35/0xff)
      wsBorderColorActive = hexColor Red
      sep = W.sepW Black 3
      klompChars = charsFitInPx dpi (fontSizePt profile) klompWidthPx

      start = [ W.wmLogNew WMLogConfig
                { titleLength = titleLen profile
                , wsImageHeight = workspaceImageHeight profile
                , titleRows = True
                , stackWsTitle = False
                }
              ]
      end = reverse
          [ W.monitorCpuW $ graphWidth profile
          , W.monitorMemW $ graphWidth profile
          , W.progressBarW
          , W.netStatsW
          , sep
          , W.netW
          , sep
          , (W.widthCharWrapW dpi (fontSizePt profile) klompChars) =<< W.klompW klompChars
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

  taffybarProfileGtkRcFile <- getUserConfigFile "taffybar" $
    "taffybar.rc." ++ name profile

  writeFile taffybarProfileGtkRcFile $ ""
        ++ "# profile: " ++ name profile ++ "\n"
        ++ "# auto-generated at: " ++ taffybarProfileGtkRcFile ++ "\n"
        ++ "\n"
        ++ "style \"taffybar-default\" {\n"
        ++ "  font_name = \"" ++ font ++ "\"\n"
        ++ "  bg[NORMAL] = \"" ++ bgColor ++ "\"\n"
        ++ "  fg[NORMAL] = \"" ++ fgColor ++ "\"\n"
        ++ "  text[NORMAL] = \"" ++ textColor ++ "\"\n"
        ++ "}\n"
        ++ "style \"taffybar-workspace-border-active\" {\n"
        ++ "  bg[NORMAL] = \"" ++ wsBorderColorActive ++ "\"\n"
        ++ "}\n"
        ++ "style \"taffybar-workspace-border-visible\" {\n"
        ++ "  bg[NORMAL] = \"" ++ wsBorderColorActive ++ "\"\n"
        ++ "}\n"
        ++ "style \"taffybar-workspace-border-empty\" {\n"
        ++ "  bg[NORMAL] = \"" ++ wsBorderColorNormal ++ "\"\n"
        ++ "}\n"
        ++ "style \"taffybar-workspace-border-hidden\" {\n"
        ++ "  bg[NORMAL] = \"" ++ wsBorderColorNormal ++ "\"\n"
        ++ "}\n"
        ++ "style \"taffybar-workspace-border-urgent\" {\n"
        ++ "  bg[NORMAL] = \"" ++ wsBorderColorNormal ++ "\"\n"
        ++ "}\n"

  defaultTaffybar cfg {startWidgets=start, endWidgets=end}

data Profile = P { name :: String
                 , height :: Int
                 , spacing :: Int
                 , titleLen :: Int
                 , typeface :: String
                 , fontSizePt :: Double
                 , graphWidth :: Int
                 , workspaceImageHeight :: Int
                 }
