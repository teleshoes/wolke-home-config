module WorkspaceImages (getIcon, loadIconInitialState) where
import Utils (getExactImageDir, selectClosestImageSize, tryMaybe)

import GI.GdkPixbuf.Objects.Pixbuf (Pixbuf, pixbufGetHeight, pixbufNewFromFile)
import GI.Gtk.Enums (Orientation(..))

import StatusNotifier.Tray (scalePixbufToSize)
import System.Taffybar.Widget.Workspaces (
  WindowData(..), WindowIconPixbufGetter,
  constantScaleWindowIconPixbufGetter, getWindowIconPixbufFromClass,
  getWindowIconPixbufFromDesktopEntry, getWindowIconPixbufFromEWMH)
import System.Taffybar.Util ((<|||>))
import System.Directory (listDirectory)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Int (Int32)
import Data.List (dropWhile, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.Utils (replace)
import Data.Maybe (fromMaybe)
import Data.Maybe (listToMaybe, catMaybes)
import Text.Regex.PCRE ((=~))

import System.Environment (getEnv)

data IconInitialState = IconInitialState { namedIconFileMap :: [(IconName, FilePath)]
                                         , wsImageHeight :: Int
                                         } deriving Show
type IconName = String

wsImageDir :: String -> String
wsImageDir imgDir = imgDir ++ "/workspace-images"

getIcon :: IconInitialState -> WindowIconPixbufGetter
getIcon state =
  scale (getWindowIconPixbufOverride state) <|||>
  scale (getWindowIconPixbufFromEWMH) <|||>
  scale (getWindowIconPixbufTitleClass state) <|||>
  scale (getWindowIconPixbufFromDesktopEntry) <|||>
  scale (getWindowIconPixbufFromClass) <|||>
  scale (getWindowIconPixbufUnknown state) <|||>
  error "could not apply icon"
  where scale = scalePixbuf size
        size = fromIntegral $ wsImageHeight state :: Int32

loadIconInitialState :: Int -> IO IconInitialState
loadIconInitialState desiredHeight = do
  imgSize <- selectClosestImageSize desiredHeight
  imgDir <- getExactImageDir imgSize
  let dir = wsImageDir imgDir
  files <- fmap (fromMaybe []) $ tryMaybe $ listDirectory dir
  let pngs = filter (".png" `isSuffixOf`) files
      names = map (map toLower . reverse . drop 4 . reverse) pngs
      filePaths = map (\name -> dir ++ "/" ++ name ++ ".png") names
  return $ IconInitialState { namedIconFileMap = zip names filePaths
                            , wsImageHeight = imgSize
                            }

scalePixbuf :: Int32 -> WindowIconPixbufGetter -> WindowIconPixbufGetter
scalePixbuf size getter getterArgSize getterArgWindowData = do
  maybePixbuf <- getter getterArgSize getterArgWindowData
  case maybePixbuf of
    Just pixbuf -> liftIO $ fmap Just $ ensurePixbufScaled size pixbuf
    Nothing     -> return Nothing

ensurePixbufScaled :: Int32 -> Pixbuf -> IO Pixbuf
ensurePixbufScaled size pixbuf = do
  h <- pixbufGetHeight pixbuf
  if h == size
  then return pixbuf
  else scalePixbufToSize size OrientationHorizontal pixbuf

getWindowIconPixbufOverride :: IconInitialState -> WindowIconPixbufGetter
getWindowIconPixbufOverride state _ windowData = liftIO $ loadFilePixbuf fileName
  where (winTitle, winClass) = (windowTitle windowData, windowClass windowData)
        overrideIconName = getOverrideIconName winTitle winClass
        fileName = iconNameToFile state overrideIconName

getWindowIconPixbufTitleClass :: IconInitialState -> WindowIconPixbufGetter
getWindowIconPixbufTitleClass state _ windowData = liftIO $ loadFilePixbuf fileName
  where (winTitle, winClass) = (windowTitle windowData, windowClass windowData)
        titleClassIconName = getTitleClassIconName state winTitle winClass
        fileName = iconNameToFile state titleClassIconName

getWindowIconPixbufUnknown :: IconInitialState -> WindowIconPixbufGetter
getWindowIconPixbufUnknown state _ _ = liftIO $ loadFilePixbuf fileName
  where fileName = iconNameToFile state $ Just "unknown"

getWindowIconPixbufNothing _ _ = return Nothing

loadFilePixbuf :: Maybe FilePath -> IO (Maybe Pixbuf)
loadFilePixbuf Nothing = return Nothing
loadFilePixbuf (Just file) = pixbufNewFromFile file

getOverrideIconName :: String -> String -> Maybe IconName
getOverrideIconName winTitle winClass
  | null winTitle && null winClass              = Just "blank"
  | winTitle =~ "Riot.*\\|.* - Mozilla Firefox" = Just "riot"
  | winTitle == "..." && winClass == "..."      = Just "blank"
  | winTitle `endsWith` " - VIM"                = Just "vim"
  | winTitle `contains` "| Jitsi Meet -"        = Just "jitsi"
  | winTitle `contains` "email-gui.py"          = Just "qtemail"
  | winTitle `contains` "qtemail-daemon"        = Just "qtemail"
  | winTitle `contains` "Tor Browser"           = Just "torbrowserbundle"
  | winTitle `contains` "Vidalia Control Panel" = Just "torbrowserbundle"
  | winTitle `ffPage` "Google Hangouts"         = Just "googlehangouts"
  | winTitle `ffPage` "escribe"                 = Just "escribe"
  | dotClass == "Navigator.Firefox"             = Just "firefox"
  | dotClass == "Pidgin.Pidgin"                 = Just "pidgin"
  | dotClass == "urxvt.URxvt"                   = Just "terminal"
  | winTitle == "Project OutFox"                = Just "stepmania"
  | otherwise = Nothing
  where ffPage s pageName = s `startsWith` pageName && s `endsWith` "MozillaFirefox"
        dotClass = classJoinDot winClass

getTitleClassIconName :: IconInitialState -> String -> String -> Maybe IconName
getTitleClassIconName state winTitle winClass = listToMaybe iconNames
  where titleIconName = searchNamedIconFileMap state winTitle
        classIconName = searchNamedIconFileMap state winClass
        iconNames = catMaybes [titleIconName, classIconName]

iconNameToFile :: IconInitialState -> Maybe String -> Maybe FilePath
iconNameToFile state (Just iconName) = lookup iconName $ namedIconFileMap state
iconNameToFile _ Nothing = Nothing

searchNamedIconFileMap :: IconInitialState -> String -> Maybe IconName
searchNamedIconFileMap state query = listToMaybe matchingIconNames
  where iconNames = map fst fileMap :: [IconName]
        lcQuery = map toLower query
        fileMap = namedIconFileMap state
        matchingIconNames = filter (lcQuery `contains`) iconNames

contains = flip isInfixOf :: String -> String -> Bool
startsWith = flip isPrefixOf :: String -> String -> Bool
endsWith = flip isSuffixOf :: String -> String -> Bool

--remove trailing NUL chars and replace all other NUL chars with '.'
--  (window classes are two NUL-terminated strings concatenated together)
--  e.g.: "Navigator\0Firefox\0" => "Navigator.Firefox"
--        "\0\0\0a.b.c.\0\0\0    => "...a.b.c."
classJoinDot :: String -> String
classJoinDot = replace "\0" "." . reverse . dropWhile (=='\0') . reverse
