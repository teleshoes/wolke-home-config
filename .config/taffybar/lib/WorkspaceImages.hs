{-# LANGUAGE FlexibleContexts #-}
module WorkspaceImages (getIcons, selectImage, selectImageName) where
import Utils (imageDir, tryMaybe)

import System.Directory (listDirectory)
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
      names = map (reverse . drop 4 . reverse) pngs
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
  | otherwise = Nothing

selectImageName :: [String] -> String -> String -> Maybe String
selectImageName imgNames winTitle winClass = listToMaybe $ catMaybes maybeNames
  where nTitle = listToMaybe $ filter (winTitle ~~) imgNames
        nClass = listToMaybe $ filter (winClass ~~) imgNames
        nSpecial = getSpecial winTitle winClass
        nUnknown = Just "unknown"
        maybeNames = [nSpecial, nClass, nTitle, nUnknown]

selectImage :: [(String, FilePath)] -> String -> String -> Maybe FilePath
selectImage icons winTitle winClass = maybeFilePath maybeName
  where imageNames = map fst icons
        maybeName = selectImageName imageNames winTitle winClass
        maybeFilePath (Just name) = lookup name icons
        maybeFilePath Nothing = Nothing
