import qualified Widgets as W
import Color (Color(..), hexColor)
import Utils (attemptCreateSymlink, chompFile, colW,
  fmtSimpleRecord, getHomeFile, maybeJoin, readInt, regexFirstGroup, tryMaybe)
import Width (charsFitInPx, getScreenDPI, screenPctToPx)

import Control.Monad.Trans (liftIO)
import System.Taffybar.SimpleConfig (simpleTaffybar, defaultSimpleTaffyConfig,
  barHeight, barPosition, widgetSpacing, startWidgets, endWidgets,
  Position(Top, Bottom))

import Data.Functor ((<$>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir ( getUserConfigFile )

typeface = "Inconsolata"
minFont = 4.0
minBarHeight = 20

getResconfigFile = getHomeFile "resconfig-screen"
getMachineTypeFile = getHomeFile "machine-type"

data Profile = P { pName :: String --profile name
                 , barHt :: Int    --bar height in pixels
                 , wImgH :: Int    --workspace image height in pixels
                 , space :: Int    --widget spacing in pixels
                 , wSepW :: Int    --widget separator width in pixels
                 , title :: Int    --window title length in characters
                 , fontP :: Double --font point size
                 , graph :: Int    --width of graphs in pixels
                 , music :: Double --percent of the screen width to use for song info
                 , appSq :: Int    --app launcher square height
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
  , barHt = int $ barHeight
  , wImgH = int $ atMost (barHeight-12) $ scaleFHD_WQHD 24 28
  , space = int $ ifTiny 1 5
  , wSepW = int $ 2 + (scaleLAP_WALL 0 1) + (scaleFHD_WQHD 0 1)
  , title = int $ atLeast 5 $ atMost 30 $ scaleHD_FHD 20 30
  , fontP = dbl $ atLeast minFont $ scaleFHD_WQHD 13.0 17.0 + scaleLAP_WALL 0 3.0
  , graph = int $ atLeast 10 $ scaleFHD_WQHD 50 80
  , music = dbl $ ifTiny 5 $ scale14IN_60IN 15.94 19.43
  , appSq = int $ barHeight
  }
  where scaleFHD_WQHD = scale (widthPx resconfig, 1920, 2560)
        scaleHD_FHD = scale (widthPx resconfig, 1280, 1920)
        scaleLAP_WALL = scale (distanceMM resconfig, 500, 1700)
        scale14IN_60IN = scale (widthMM resconfig, 310, 2253)
        screenBig = widthPx resconfig > 1920 || distanceMM resconfig > 1000
        screenTiny = widthPx resconfig < 1280 || distanceMM resconfig < 300
        ifBig = \x y -> if screenBig then x else y
        ifTiny = \x y -> if screenTiny then x else y
        barHeight = atLeast minBarHeight $ scaleFHD_WQHD 38 50 + scaleLAP_WALL 0 10
        atLeast = max
        atMost = min
        int = round :: RealFrac n => n -> Int
        dbl = id :: Double -> Double

scale :: RealFrac n => (Int, Int, Int) -> n -> n -> n
scale (factorVal, factorLow, factorHigh) low high = low + factor*(high-low)
  where factor = (fVal - fLow) / (fHigh - fLow)
        [fVal, fLow, fHigh] = map fromIntegral [factorVal, factorLow, factorHigh]

main = do
  now <- getCurrentTime
  resconfig <- readResconfigScreen
  machineType <- readMachineType

  let profile = calculateProfile resconfig
  putStrLn $ "machine type: " ++ machineType
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
      sep = W.sepW $ wSepW profile
      klompChars = charsFitInPx dpi (fontP profile) klompWidthPx

      all = Just
      main w = if machineType == "main" then Just w else Nothing
      tv w = if machineType == "tv" then Just w else Nothing
      lap w = if machineType /= "tv" then Just w else Nothing

      start = catMaybes
              [ Nothing
              , all $ W.workspaceSwitcherW $ wImgH profile
              , all $ W.windowTitleW (title profile) 2
              , all $ liftIO $ sep
              , all $ W.layoutWindowsW
              ]
      end = map liftIO $ catMaybes $ reverse
          [ Nothing
          , all  $ W.monitorCpuW $ graph profile
          , all  $ W.monitorMemW $ graph profile
          , all  $ W.syncWatchW
          , all  $ W.progressBarW
          , all  $ W.netStatsW
          , all  $ sep
          , all  $ W.netW
          , lap  $ sep
          , lap  $ W.fcrondynW
          , all  $ sep
          , all  $ (W.widthCharWrapW dpi (fontP profile) klompChars) =<< W.klompW klompChars
          , tv   $ sep
          , tv   $ W.speakerW
          , all  $ W.volumeW
          , all  $ W.micW
          , main $ W.pidginPipeW $ appSq profile
          , main $ W.qtemailW $ appSq profile
          , all  $ W.cpuScalingSimpleW
          , all  $ W.cpuFreqsW
          , lap  $ W.fanW
          , all  $ W.brightnessW
          , tv   $ W.screenSaverW
          , all  $ colW $ catMaybes [ all  $ W.pingMonitorW "G" "www.google.com"
                                    , main $ W.pingMonitorW "E" "ehr.dev"
                                    ]
          , main $ W.openvpnW "lg" "VPN\nlg "
          , lap  $ W.tpBattStatW $ barHt profile
          , all  $ sep
          , all  $ W.clockW
          ]

  cssProfileFile <- getUserConfigFile "taffybar" "taffybar-profile.css"

  let iconHackPad = fromIntegral $ barHt profile - wImgH profile - 4
      iconHackPadTop = round $ iconHackPad / 2
      iconHackPadBot = iconHackPadTop - 2 --margin-bottom is 2px

  writeFile cssProfileFile $ ""
        ++ "/* Taffybar Profile CSS\n"
        ++ " *\n"
        ++ " * auto-generated at:\n"
        ++ " *   " ++ show now ++ "\n"
        ++ " * in file:\n"
        ++ " *   " ++ cssProfileFile ++ "\n"
        ++ " *\n"
        ++ " * profile:\n"
        ++ " *   " ++ fmtSimpleRecord profile " *   "
        ++ " * resconfig-screen:\n"
        ++ " *   " ++ fmtSimpleRecord resconfig " *   "
        ++ " * machine-type:\n"
        ++ " *   " ++ machineType ++ "\n"
        ++ " */\n"
        ++ "@define-color bgColor " ++ bgColor ++ ";\n"
        ++ "@define-color textColor " ++ textColor ++ ";\n"
        ++ "@define-color wsBorderColorNormal " ++ wsBorderColorNormal ++ ";\n"
        ++ "@define-color wsBorderColorActive " ++ wsBorderColorActive ++ ";\n"
        ++ "label {\n"
        ++ "  font: " ++ (show $ fontP profile) ++ "pt " ++ show typeface ++ ";\n"
        ++ "}\n"
        ++ "/** workspace icon padding hack */\n"
        ++ ".contents {\n"
        ++ "  padding-top: " ++ show iconHackPadTop ++ "px;\n"
        ++ "  padding-bottom: " ++ show iconHackPadBot ++ "px;\n"
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

readMachineType :: IO String
readMachineType = fmap (fromMaybe "main") $ tryMaybe $ do
  machineTypeFile <- getMachineTypeFile
  chompFile machineTypeFile
