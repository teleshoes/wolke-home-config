module WorkspaceImages (loadImages, selectImage, selectImageName) where

import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import GHC.Word (Word8)
import Control.Exception (try, SomeException)
import Data.Maybe (listToMaybe, catMaybes)

import Text.Regex.PCRE (
  Regex, match, makeRegexOpts, compCaseless, defaultExecOpt)
import System.Environment (getEnv)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf, pixbufNewFromFile, pixbufAddAlpha)

(imgWidth, imgHeight) = (20, 20)

getImageFile :: String -> IO String
getImageFile name = do
  dir <- getImageDir
  return $ dir ++ "/" ++ name ++ ".png"

getImageNames :: IO [String]
getImageNames = do
  dir <- getImageDir
  files <- handle $ getDirectoryContents dir
  case files of
    Just fs -> return $ catMaybes $ map getPng fs
    Nothing -> return []
  where getPng f = if ".png" `isSuffixOf` f then Just $ stripPng f else Nothing
        stripPng = reverse . drop 4 . reverse

getImageDir :: IO String
getImageDir = do
  home <- getEnv "HOME"
  return $ ""
           ++ home ++ "/.config/taffybar/icons/workspace-images"
           ++ "/" ++ show imgWidth ++ "x" ++ show imgHeight

tryAnything :: (IO a) -> IO (Either SomeException a)
tryAnything = try

handle :: (IO a) -> IO (Maybe a)
handle act = do
  result <- tryAnything act
  case result of
    Left ex  -> do
      print $ show ex
      return Nothing
    Right val -> return $ Just val

loadImage :: String -> IO (Maybe Pixbuf)
loadImage name = handle $ pixbufNewFromFile =<< getImageFile name


addAlphaWhite = addAlpha $ Just (65535, 65535, 65535)
addAlphaBlack = addAlpha $ Just (0, 0, 0)

addAlpha :: Maybe (Word8, Word8, Word8) -> Maybe Pixbuf -> IO (Maybe Pixbuf)
addAlpha color Nothing = return Nothing
addAlpha color (Just pb) = fmap Just $ pixbufAddAlpha pb color

loadImages :: IO [(String, Maybe Pixbuf)]
loadImages = do
  imageNames <- getImageNames
  pixbufs <- mapM loadImage imageNames
  return $ zip imageNames pixbufs

(~~) s re = match regex s :: Bool
  where regex = makeRegexOpts compCaseless defaultExecOpt re :: Regex

getSpecial winTitle winClass
  | null winTitle && null winClass = Just "blank"
  | winTitle ~~ "Tor Browser|Vidalia Control Panel" = Just "torbrowserbundle"
  | winTitle ~~ "^eScribe .*- Mozille Firefox$" = Just "escribe"
  | winTitle ~~ " \\| Review Board - Mozille Firefox$" = Just "reviewboard"
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
selectImage _ Nothing = Nothing
selectImage pixbufs (Just (winTitle, winClass)) = pb
  where imageNames = map fst pixbufs
        maybeName = selectImageName imageNames winTitle winClass
        pb = case maybeName of
          Just name -> getPixbuf pixbufs name
          Nothing -> Nothing
