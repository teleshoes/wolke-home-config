import qualified Widgets as W
import Color (Color(..), hexColor)
import Utils (attemptCreateSymlink, colW,
  getHomeFile, maybeJoin, readInt, regexFirstGroup, tryMaybe)
import Width (charsFitInPx, getScreenDPI, screenPctToPx)

import Control.Monad.Trans (liftIO)
import System.Taffybar.SimpleConfig (simpleTaffybar, defaultSimpleTaffyConfig,
  barHeight, barPosition, widgetSpacing, startWidgets, endWidgets,
  Position(Top, Bottom))

import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir ( getUserConfigFile )

typeface = "Inconsolata"

getResconfigFile = getHomeFile "resconfig-screen"

data Profile = P { pName :: String --profile name
                 , barHt :: Int    --bar height in pixels
                 , wImgH :: Int    --workspace image height in pixels
                 , space :: Int    --widget spacing in pixels
                 , wSepW :: Int    --widget separator width in pixels
                 , title :: Int    --window title length in characters
                 , fontP :: Double --font point size
                 , graph :: Int    --width of graphs in pixels
                 , music :: Double --percent of the screen width to use for song info
                 } deriving Show

data Resconfig = R { name       :: String --screen/monitor name
                   , widthPx    :: Int    --horizontal resolution
                   , heightPx   :: Int    --vertical resolution
                   , widthMM    :: Int    --horizontal screen size
                   , heightMM   :: Int    --vertical screen size
                   , distanceMM :: Int    --expected viewing distance
                   } deriving Show

calculateProfile resconfig = P
  { pName = name resconfig
  , barHt = int $ scaleFHD_WQHD 38 50 + scaleLAP_WALL 0 10
  , wImgH = int $ if isBig then 28 else 24
  , space = int $ 5
  , wSepW = int $ 2 + (scaleLAP_WALL 0 1) + (scaleFHD_WQHD 0 1)
  , title = int $ 30
  , fontP = dbl $ scaleFHD_WQHD 13.0 17.0 + scaleLAP_WALL 0 3.0
  , graph = int $ scaleFHD_WQHD 50 80
  , music = dbl $ scale14IN_60IN 15.94 19.43
  }
  where scaleFHD_WQHD = scale (widthPx resconfig, 1920, 2560)
        scaleLAP_WALL = scale (distanceMM resconfig, 500, 1700)
        scale14IN_60IN = scale (widthMM resconfig, 310, 2253)
        isBig = widthPx resconfig > 1920 || distanceMM resconfig > 1000
        int = round :: RealFrac n => n -> Int
        dbl = id :: Double -> Double

scale :: RealFrac n => (Int, Int, Int) -> n -> n -> n
scale (factorVal, factorLow, factorHigh) low high = low + factor*(high-low)
  where factor = (fVal - fLow) / (fHigh - fLow)
        [fVal, fLow, fHigh] = map fromIntegral [factorVal, factorLow, factorHigh]

main = do
  resconfig <- readResconfigScreen
  let profile = calculateProfile resconfig
  print resconfig
  print profile
  dpi <- getScreenDPI
  isBot <- elem "--bottom" <$> getArgs
  klompWidthPx <- screenPctToPx $ music profile
  let cfg = defaultSimpleTaffyConfig { barHeight = barHt profile
                                     , widgetSpacing = space profile
                                     , barPosition = if isBot then Bottom else Top
                                     }
      bgColor = hexColor $ RGB (0x00/0xff, 0x2b/0xff, 0x36/0xff)
      textColor = hexColor $ RGB (0x93/0xff, 0xa1/0xff, 0xa1/0xff)
      wsBorderColorNormal = hexColor $ RGB (0xD4/0xff, 0xAD/0xff, 0x35/0xff)
      wsBorderColorActive = hexColor Red
      sep = W.sepW Black $ wSepW profile
      klompChars = charsFitInPx dpi (fontP profile) klompWidthPx


      start = [ W.workspaceSwitcherW $ wImgH profile
              , W.windowTitleW (title profile) 2
              , liftIO $ sep
              , W.layoutWindowsW
              ]
      end = map liftIO $ reverse
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
          , W.cpuScalingSimpleW
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

  simpleTaffybar cfg {startWidgets=start, endWidgets=end}

readResconfigScreen :: IO Resconfig
readResconfigScreen = fmap (fromMaybe defaultResconfig) $ tryMaybe $ do
  resconfigFile <- getResconfigFile
  str <- readFile resconfigFile
  return $ R { name       = def name       $ getStr "name" str
             , widthPx    = def widthPx    $ getInt "width_px" str
             , heightPx   = def heightPx   $ getInt "height_px" str
             , widthMM    = def widthMM    $ getInt "width_mm" str
             , heightMM   = def heightMM   $ getInt "height_mm" str
             , distanceMM = def distanceMM $ getInt "distance_mm" str
             }
  where defaultResconfig = R "default" 1920 1080 344 194 500
        def field = fromMaybe $ field defaultResconfig
        getStr :: String -> String -> Maybe String
        getStr field str = regexFirstGroup (field ++ "\\s*=\\s*(.*)") str
        getInt :: String -> String -> Maybe Int
        getInt field str = fmap fromIntegral $ maybeJoin readInt $ getStr field str
