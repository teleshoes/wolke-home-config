module WorkspaceImages (getIcon, loadNamedIconFileMap, loadPlaceholderPixbuf) where
import Utils (imageDir, tryMaybe)

import GI.GdkPixbuf.Objects.Pixbuf (
  Pixbuf, pixbufFill, pixbufNew, pixbufNewFromFile, pixbufScaleSimple)
import GI.GdkPixbuf.Enums (Colorspace(..), InterpType(..))
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Workspaces (
  WorkspacesIO, WindowData(..),
  getWindowIconPixbufFromEWMH, liftX11Def)
import System.Directory (listDirectory)
import Control.Exception (try, SomeException)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Int (Int32)
import Data.Text (stripSuffix)
import Data.List (dropWhile, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.Utils (replace)
import Data.Maybe (fromMaybe)
import Data.Maybe (listToMaybe, catMaybes)

import System.Environment (getEnv)

type NamedIconFileMap = [(String, FilePath)]
type IconName = String

getIcon :: NamedIconFileMap -> Int -> Maybe Pixbuf -> Int32 -> WindowData -> TaffyIO (Maybe Pixbuf)
getIcon namedIconFileMap wsImageHeight placeholderPixbuf whoknows windowData = applyUntilJust getIconsOrder
  where getIconsOrder = [getOverrideIcon, getEwmhIcon, getTitleClassIcon, getPlaceholderIcon]
        getOverrideIcon = liftIO $ loadFilePixbuf $ nameToFile $ overrideIconName
        getEwmhIcon = loadEWMHPixbuf wsImageHeight windowData
        getTitleClassIcon = liftIO $ loadFilePixbuf $ nameToFile $ titleClassIconName
        getPlaceholderIcon = return placeholderPixbuf

        overrideIconName = getOverrideIconName winTitle winClass
        titleClassIconName = getTitleClassIconName namedIconFileMap winTitle winClass

        nameToFile (Just iconName) = lookup iconName namedIconFileMap
        nameToFile Nothing = Nothing
        (winTitle, winClass) = (windowTitle windowData, windowClass windowData)

loadEWMHPixbuf :: Int -> WindowData -> TaffyIO (Maybe Pixbuf)
loadEWMHPixbuf wsImageHeight windowData = scale =<< getEWMHPixbuf
  where size = fromIntegral wsImageHeight :: Int32
        scale Nothing = return Nothing
        scale (Just pixbuf) = pixbufScaleSimple pixbuf size size InterpTypeBilinear
        getEWMHPixbuf = getWindowIconPixbufFromEWMH size windowData

loadFilePixbuf :: Maybe FilePath -> IO (Maybe Pixbuf)
loadFilePixbuf (Just file) = maybeApply $ pixbufNewFromFile file
loadFilePixbuf Nothing = return Nothing

loadPlaceholderPixbuf :: Int -> IO (Maybe Pixbuf)
loadPlaceholderPixbuf wsImageHeight = clearPixbuf =<< pixbufNewEmpty size size
  where size = fromIntegral wsImageHeight :: Int32
        pixbufNewEmpty w h = pixbufNew ColorspaceRgb True 8 w h
        clearPixbuf Nothing = return Nothing
        clearPixbuf (Just pixbuf) = do
          pixbufFill pixbuf 0x0000000
          return $ Just pixbuf

maybeApply :: (IO a) -> IO (Maybe a)
maybeApply act = do
  result <- tryAnything act
  case result of
    Left ex  -> do
      print $ show ex
      return Nothing
    Right val -> return $ Just val

tryAnything :: (IO a) -> IO (Either SomeException a)
tryAnything = try

applyUntilJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
applyUntilJust [] = return Nothing
applyUntilJust (act:acts) = do
  res <- act
  case res of
    Just val -> return $ Just val
    Nothing  -> applyUntilJust acts

loadNamedIconFileMap :: Int -> IO NamedIconFileMap
loadNamedIconFileMap h = do
  dir <- wsImageDir h
  files <- fmap (fromMaybe []) $ tryMaybe $ listDirectory dir
  let pngs = filter (".png" `isSuffixOf`) files
      names = map (map toLower . reverse . drop 4 . reverse) pngs
      filePaths = map (\name -> dir ++ "/" ++ name ++ ".png") names
  return $ zip names filePaths

wsImageDir :: Int -> IO String
wsImageDir h = fmap (++ "/workspace-images") $ imageDir h

getOverrideIconName :: String -> String -> Maybe IconName
getOverrideIconName winTitle winClass
  | null winTitle && null winClass              = Just "blank"
  | winTitle == "..." && winClass == "..."      = Just "blank"
  | winTitle `endsWith` " - VIM"                = Just "vim"
  | winTitle `contains` "email-gui.py"          = Just "qtemail"
  | winTitle `contains` "qtemail-daemon"        = Just "qtemail"
  | winTitle `contains` "Tor Browser"           = Just "torbrowserbundle"
  | winTitle `contains` "Vidalia Control Panel" = Just "torbrowserbundle"
  | winTitle `ffPage` "Google Hangouts"         = Just "googlehangouts"
  | winTitle `ffPage` "escribe"                 = Just "escribe"
  | dotClass == "Pidgin.Pidgin"                 = Just "pidgin"
  | dotClass == "urxvt.URxvt"                   = Just "terminal"
  | otherwise = Nothing
  where ffPage s pageName = s `startsWith` pageName && s `endsWith` "MozillaFirefox"
        dotClass = classJoinDot winClass

getTitleClassIconName :: NamedIconFileMap -> String -> String -> Maybe IconName
getTitleClassIconName namedIconFileMap winTitle winClass = listToMaybe iconNames
  where titleIconName = searchNamedIconFileMap namedIconFileMap winTitle
        classIconName = searchNamedIconFileMap namedIconFileMap winClass
        unknownIconName = Just "unknown"
        iconNames = catMaybes [titleIconName, classIconName, unknownIconName]

searchNamedIconFileMap :: NamedIconFileMap -> String -> Maybe IconName
searchNamedIconFileMap namedIconFileMap query = listToMaybe matchingIconNames
  where iconNames = map fst namedIconFileMap :: [IconName]
        lcQuery = map toLower query
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
