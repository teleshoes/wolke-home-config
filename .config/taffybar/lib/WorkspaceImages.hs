module WorkspaceImages (loadImages, selectImage, selectImageName) where

import GHC.Word (Word8)
import Control.Exception (try, SomeException)
import Data.Maybe (listToMaybe, catMaybes)

import Text.Regex.PCRE (
  Regex, match, makeRegexOpts, compCaseless, defaultExecOpt)
import System.Environment (getEnv)
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf, pixbufNewFromFile, pixbufAddAlpha)

images = [ "blank"
         , "downloads"
         , "eclipse"
         , "eog"
         , "escribe"
         , "gimp"
         , "gwt"
         , "klomp"
         , "mplayer"
         , "navigator"
         , "pidgin"
         , "reviewboard"
         , "rhythmbox"
         , "stepmania"
         , "terminal"
         , "thunderbird"
         , "torbrowserbundle"
         , "transmission"
         , "unknown"
         , "vim"
         ]

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
loadImage name = do
  home <- getEnv "HOME"
  let dir = home ++ "/.config/taffybar/icons/workspace-images"
      file = dir ++ "/" ++ name ++ ".xpm"
  pb <- handle $ pixbufNewFromFile $ file
  addAlphaWhite pb

addAlphaWhite = addAlpha $ Just (65535, 65535, 65535)
addAlphaBlack = addAlpha $ Just (0, 0, 0)

addAlpha :: Maybe (Word8, Word8, Word8) -> Maybe Pixbuf -> IO (Maybe Pixbuf)
addAlpha color Nothing = return Nothing
addAlpha color (Just pb) = fmap Just $ pixbufAddAlpha pb color

loadImages :: IO [(String, Maybe Pixbuf)]
loadImages = do
  pixbufs <- mapM loadImage images
  return $ zip images pixbufs

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


selectImageName :: String -> String -> Maybe String
selectImageName winTitle winClass = listToMaybe $ catMaybes maybeNames
  where nTitle = listToMaybe $ filter (winTitle ~~) images :: Maybe String
        nClass = listToMaybe $ filter (winClass ~~) images :: Maybe String
        nSpecial = getSpecial winTitle winClass :: Maybe String
        nUnknown = Just "unknown"
        maybeNames = [nSpecial, nClass, nTitle, nUnknown]

selectImage :: [(String, Maybe Pixbuf)] -> Maybe (String, String) -> Maybe Pixbuf
selectImage _ Nothing = Nothing
selectImage pixbufs (Just (winTitle, winClass)) = pb
  where maybeName = selectImageName winTitle winClass
        pb = case maybeName of
          Just name -> getPixbuf pixbufs name
          Nothing -> Nothing
