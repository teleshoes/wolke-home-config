module WorkspaceImages (getIcon, loadNamedIconFileMap) where
import Utils (imageDir, tryMaybe)

import System.Taffybar.Widget.Workspaces (
  WorkspacesIO, IconInfo(..), WindowData(..),
  defaultGetIconInfo, liftX11Def)
import System.Taffybar.Information.EWMHDesktopInfo (
  EWMHIconData, getWindowIconsData)

import System.Directory (listDirectory)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Text (stripSuffix)
import Data.List (dropWhile, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.Utils (replace)
import Data.Maybe (fromMaybe)
import Data.Maybe (listToMaybe, catMaybes)

import           System.Taffybar.Information.EWMHDesktopInfo
import System.Environment (getEnv)

type NamedIconFileMap = [(String, FilePath)]
type IconName = String

getIcon :: NamedIconFileMap -> WindowData -> WorkspacesIO IconInfo
getIcon namedIconFileMap windowData = do
  icon <- applyUntilJust [getOverrideIcon, getEwmhIcon, getTitleClassIcon]
  return $ fromMaybe IINone icon
  where getOverrideIcon = return $ fmap IIFilePath $ nameToFile $ overrideIconName
        getEwmhIcon = fmap (fmap IIEWMH) $ getEwmhIconData windowData
        getTitleClassIcon = return $ fmap IIFilePath $ nameToFile $ titleClassIconName

        overrideIconName = getOverrideIconName winTitle winClass
        titleClassIconName = getTitleClassIconName namedIconFileMap winTitle winClass

        nameToFile (Just iconName) = lookup iconName namedIconFileMap
        nameToFile Nothing = Nothing
        (winTitle, winClass) = (windowTitle windowData, windowClass windowData)

applyUntilJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
applyUntilJust [] = return Nothing
applyUntilJust (act:acts) = do
  res <- act
  case res of
    Just val -> return $ Just val
    Nothing  -> applyUntilJust acts

getEwmhIconData :: WindowData -> WorkspacesIO (Maybe EWMHIconData)
getEwmhIconData windowData = do 
  icon <- liftX11Def Nothing (getWindowIconsData $ windowId windowData)
  case icon of
    Just (_, 0) -> return Nothing -- empty size EWMH icon
    _           -> return icon

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
