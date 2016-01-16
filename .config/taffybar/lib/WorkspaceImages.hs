{-# LANGUAGE FlexibleContexts #-}
module WorkspaceImages (loadImages, selectImage, selectImageName) where
import Utils (imageDir, tryMaybe)

import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import GHC.Word (Word8)
import Data.Maybe (listToMaybe, catMaybes)

import Text.Regex.PCRE (
  Regex, match, makeRegexOpts, compCaseless, defaultExecOpt)
import System.Environment (getEnv)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf, pixbufNewFromFile, pixbufAddAlpha)

getImageFile :: Int -> String -> IO String
getImageFile h name = do
  dir <- wsImageDir h
  return $ dir ++ "/" ++ name ++ ".png"

getImageNames :: Int -> IO [String]
getImageNames h = do
  dir <- wsImageDir h
  files <- tryMaybe $ getDirectoryContents dir
  case files of
    Just fs -> return $ catMaybes $ map getPng fs
    Nothing -> return []
  where getPng f = if ".png" `isSuffixOf` f then Just $ stripPng f else Nothing
        stripPng = reverse . drop 4 . reverse

wsImageDir :: Int -> IO String
wsImageDir h = fmap (++ "/workspace-images") $ imageDir h

loadImage :: Int -> String -> IO (Maybe Pixbuf)
loadImage h name = tryMaybe $ pixbufNewFromFile =<< getImageFile h name


addAlphaWhite = addAlpha $ Just (255, 255, 255)
addAlphaBlack = addAlpha $ Just (0, 0, 0)

addAlpha :: Maybe (Word8, Word8, Word8) -> Maybe Pixbuf -> IO (Maybe Pixbuf)
addAlpha color Nothing = return Nothing
addAlpha color (Just pb) = fmap Just $ pixbufAddAlpha pb color

loadImages :: Int -> IO [(String, Maybe Pixbuf)]
loadImages h = do
  imageNames <- getImageNames h
  pixbufs <- mapM (loadImage h) imageNames
  return $ zip imageNames pixbufs

(~~) :: String -> String -> Bool
(~~) s re = match regex s
  where regex = makeRegexOpts compCaseless defaultExecOpt re :: Regex

getSpecial winTitle winClass
  | null winTitle && null winClass = Just "blank"
  | winTitle == "..." && winClass == "..." = Just "blank"
  | winTitle ~~ "email-gui.py" = Just "qtemail"
  | winTitle ~~ "Tor Browser|Vidalia Control Panel" = Just "torbrowserbundle"
  | winTitle ~~ "^eScribe .*- Mozilla Firefox$" = Just "escribe"
  | winTitle ~~ " - VIM$" = Just "vim"
  | winTitle ~~ " \\| Review Board - Mozille Firefox$" = Just "reviewboard"
  | winTitle ~~ "^SABnzbd \\d+\\.\\d+\\.\\d+.* - Chromium$" = Just "sabnzbd"
  | winTitle ~~ (""
                ++ "^\\d+\\.\\d+ [KMG]B/s"
                ++ " \\| " ++ "\\d+ [KMG]B"
                ++ " \\| " ++ "[0-9:]+ left - Chromium$") = Just "sabnzbd"
  | otherwise = Nothing

getPixbuf :: [(String, Maybe Pixbuf)] -> String -> Maybe Pixbuf
getPixbuf pixbufs name = listToMaybe $ catMaybes $ pbs
  where pbs = map snd $ filter ((==name).fst) pixbufs


selectImageName :: [String] -> String -> String -> Maybe String
selectImageName imgNames winTitle winClass = listToMaybe $ catMaybes maybeNames
  where nTitle = listToMaybe $ filter (winTitle ~~) imgNames
        nClass = listToMaybe $ filter (winClass ~~) imgNames
        nSpecial = getSpecial winTitle winClass
        nUnknown = Just "unknown"
        maybeNames = [nSpecial, nClass, nTitle, nUnknown]

selectImage :: [(String, Maybe Pixbuf)] -> Maybe (String, String) -> Maybe Pixbuf
selectImage pixbufs Nothing = getPixbuf pixbufs "blank"
selectImage pixbufs (Just (winTitle, winClass)) = pb
  where imageNames = map fst pixbufs
        maybeName = selectImageName imageNames winTitle winClass
        pb = getPixbuf pixbufs $ fromMaybe "blank" maybeName
