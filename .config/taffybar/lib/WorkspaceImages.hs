{-# LANGUAGE FlexibleContexts #-}
module WorkspaceImages (getIcons, selectImage, selectImageName) where
import Utils (imageDir, tryMaybe)

import System.Directory (listDirectory)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Maybe (listToMaybe, catMaybes)

import System.Environment (getEnv)

getIcons :: Int -> IO [(String, FilePath)]
getIcons h = do
  dir <- wsImageDir h
  files <- fmap (fromMaybe []) $ tryMaybe $ listDirectory dir
  let pngs = filter (".png" `isSuffixOf`) files
      names = map (map toLower . reverse . drop 4 . reverse) pngs
      filePaths = map (\name -> dir ++ "/" ++ name ++ ".png") names
  return $ zip names filePaths

wsImageDir :: Int -> IO String
wsImageDir h = fmap (++ "/workspace-images") $ imageDir h

contains = flip isInfixOf
startsWith = flip isPrefixOf
endsWith = flip isSuffixOf

getOverride winTitle winClass
  | null winTitle && null winClass              = Just "blank"
  | winTitle == "..." && winClass == "..."      = Just "blank"
  | winTitle `endsWith` " - VIM"                = Just "vim"
  | winClass `contains` "urxvt"                 = Just "terminal"
  | winTitle `contains` "email-gui.py"          = Just "qtemail"
  | winTitle `contains` "qtemail-daemon"        = Just "qtemail"
  | winTitle `contains` "Tor Browser"           = Just "torbrowserbundle"
  | winTitle `contains` "Vidalia Control Panel" = Just "torbrowserbundle"
  | winTitle `ffPage` "Google Hangouts"         = Just "googlehangouts"
  | winTitle `ffPage` "escribe"                 = Just "escribe"
  | otherwise = Nothing
  where ffPage s pageName = s `startsWith` pageName && s `endsWith` "MozillaFirefox"

selectImageName :: [String] -> Bool -> String -> String -> Maybe String
selectImageName imgNames hasIcon winTitle winClass = listToMaybe $ catMaybes maybeNames
  where nTitle = listToMaybe $ filter (lcTitle `contains`) imgNames
        nClass = listToMaybe $ filter (lcClass `contains`) imgNames
        nOverride = getOverride winTitle winClass
        nUnknown = Just "unknown"
        maybeNames = if hasIcon then [nOverride] else [nOverride, nClass, nTitle, nUnknown]
        lcTitle = map toLower winTitle
        lcClass = map toLower winClass

selectImage :: [(String, FilePath)] -> Bool -> String -> String -> Maybe FilePath
selectImage icons hasIcon winTitle winClass = maybeFilePath maybeName
  where imageNames = map fst icons
        maybeName = selectImageName imageNames hasIcon winTitle winClass
        maybeFilePath (Just name) = lookup name icons
        maybeFilePath Nothing = Nothing
