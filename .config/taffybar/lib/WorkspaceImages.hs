{-# LANGUAGE FlexibleContexts #-}
module WorkspaceImages (getIcons, selectImage, selectImageName) where
import Utils (imageDir, tryMaybe)

import System.Directory (listDirectory)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Maybe (listToMaybe, catMaybes)

import Text.Regex.PCRE (
  Regex, match, makeRegexOpts, compCaseless, defaultExecOpt)
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

(~~) :: String -> String -> Bool
(~~) s re = match regex s
  where regex = makeRegexOpts compCaseless defaultExecOpt re :: Regex

getSpecial winTitle winClass
  | null winTitle && null winClass = Just "blank"
  | winTitle == "..." && winClass == "..." = Just "blank"
  | winTitle ~~ "email-gui.py" = Just "qtemail"
  | winTitle ~~ "qtemail-daemon" = Just "qtemail"
  | winTitle ~~ "Tor Browser|Vidalia Control Panel" = Just "torbrowserbundle"
  | winTitle ~~ "^Google Hangouts .*- Mozilla Firefox$" = Just "googlehangouts"
  | winTitle ~~ "^eScribe .*- Mozilla Firefox$" = Just "escribe"
  | winTitle ~~ " - VIM$" = Just "vim"
  | winTitle ~~ " \\| Review Board - Mozille Firefox$" = Just "reviewboard"
  | winTitle ~~ "^SABnzbd \\d+\\.\\d+\\.\\d+.* - Chromium$" = Just "sabnzbd"
  | winTitle ~~ (""
                ++ "^\\d+\\.\\d+ [KMG]B/s"
                ++ " \\| " ++ "\\d+ [KMG]B"
                ++ " \\| " ++ "[0-9:]+ left - Chromium$") = Just "sabnzbd"
  | winClass ~~ "urxvt" = Just "terminal"
  | otherwise = Nothing

selectImageName :: [String] -> Bool -> String -> String -> Maybe String
selectImageName imgNames hasIcon winTitle winClass = listToMaybe $ catMaybes maybeNames
  where nTitle = listToMaybe $ filter (lcTitle `contains`) imgNames
        nClass = listToMaybe $ filter (lcClass `contains`) imgNames
        nSpecial = getSpecial winTitle winClass
        nUnknown = Just "unknown"
        maybeNames = if hasIcon then [nSpecial] else [nSpecial, nClass, nTitle, nUnknown]
        lcTitle = map toLower winTitle
        lcClass = map toLower winClass

selectImage :: [(String, FilePath)] -> Bool -> String -> String -> Maybe FilePath
selectImage icons hasIcon winTitle winClass = maybeFilePath maybeName
  where imageNames = map fst icons
        maybeName = selectImageName imageNames hasIcon winTitle winClass
        maybeFilePath (Just name) = lookup name icons
        maybeFilePath Nothing = Nothing
