{-# LANGUAGE DeriveFunctor #-}
module JsonWidget(jsonWidgetNew) where
import Clickable (clickableLeftAsync)

import Data.Maybe (listToMaybe)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, takeMVar, putMVar)
import Control.Exception (try, SomeException)
import System.Process (system)
import System.IO (stderr, hPutStrLn)

import Graphics.UI.Gtk (
  Widget, Container,
  castToLabel, castToContainer, castToImage, toWidget,
  containerAdd, containerRemove, containerForeach,
  hBoxNew, labelNew, imageNew,
  labelSetMarkup, imageSetFromFile,
  widgetShowAll, postGUISync)
import Text.JSON (JSObject, Result(Ok, Error), decode, fromJSObject)

data GuiMarkupF a = Click a | Label a | Image a
  deriving (Show, Read, Eq, Ord, Functor)
type GuiMarkup = GuiMarkupF String

data GuiState = GuiState
  { guiMarkups     :: [GuiMarkup]
  , widgets         :: [Widget]
  , panel           :: Container
  , clickCommand    :: Maybe String
  }


parseJsonPair :: (String, String) -> GuiMarkup
parseJsonPair (name, value) = case name of
  "click" -> Click value
  "label" -> Label value
  "image" -> Image value

parseJson :: String -> Either String [GuiMarkup]
parseJson str = case res of
                  Error s -> Left s
                  Ok obj -> Right $ map parseJsonPair $ fromJSObject obj
  where res = decode str :: Result (JSObject String)

readPrinterJson :: (IO String) -> IO (Either String [GuiMarkup])
readPrinterJson printer = do
  jsonStr <- printer
  return $ parseJson jsonStr

tryAnything :: (IO a) -> IO (Either SomeException a)
tryAnything = try

maybeReadPrinterJson :: (IO String) -> IO (Either String [GuiMarkup])
maybeReadPrinterJson printer = do
  result <- tryAnything $ readPrinterJson printer
  return $ case result of
    Left ex  -> Left $ show ex
    Right val -> val

clearContainer :: Container -> IO ()
clearContainer c = containerForeach c (containerRemove c)

guiError :: String -> (MVar GuiState) -> IO ()
guiError error guiStateMVar = do
  guiState <- takeMVar guiStateMVar
  hPutStrLn stderr error
  let p = panel guiState
  clearContainer p
  errorLabel <- fmap toWidget $ labelNew $ Just error
  containerAdd p errorLabel
  widgetShowAll p
  putMVar guiStateMVar guiState { guiMarkups = []
                                , widgets = [errorLabel]
                                , clickCommand = Nothing
                                }

parseClickCommand :: [GuiMarkup] -> (Maybe String, [GuiMarkup])
parseClickCommand gms = (cmd, widgetGuiMarkups)
  where clickGuiMarkup = listToMaybe $ filter isClickCmd gms
        cmd = case clickGuiMarkup of
          Nothing -> Nothing
          Just (Click cmd) -> Just cmd
        widgetGuiMarkups = filter (not . isClickCmd) gms
        isClickCmd x = case x of
          (Click cmd) -> True
          otherwise   -> False

buildWidget :: GuiMarkup -> IO Widget
buildWidget gm = case gm of
  (Label text) -> fmap toWidget $ labelNew (Nothing :: Maybe String)
  (Image img) -> fmap toWidget imageNew

sameWidgetStructure :: [GuiMarkup] -> [GuiMarkup] -> Bool
sameWidgetStructure xs ys = (map void xs) == (map void ys)

buildWidgets :: [GuiMarkup] -> (MVar GuiState) -> IO ()
buildWidgets gms guiStateMVar = do
  guiState <- takeMVar guiStateMVar
  let prevGMs = guiMarkups guiState
  let (clickCmd, newGMs) = parseClickCommand gms
  ws <- if sameWidgetStructure prevGMs newGMs then do
          return $ widgets guiState
        else do
          let p = panel guiState
          newWs <- mapM buildWidget newGMs
          clearContainer p
          mapM (containerAdd p) newWs
          widgetShowAll p
          return newWs
  putMVar guiStateMVar guiState { guiMarkups=newGMs
                                , widgets=ws
                                , clickCommand=clickCmd
                                }

updateWidget :: (Widget, GuiMarkup) -> IO ()
updateWidget (w,gm) = case gm of
  (Label text) -> labelSetMarkup (castToLabel w) text
  (Image image) -> imageSetFromFile (castToImage w) image

updateWidgets guiStateMVar = do
  guiState <- readMVar guiStateMVar
  mapM updateWidget $ zip (widgets guiState) (guiMarkups guiState)
  return ()

updateGuiStateLoop :: (IO String) -> (MVar GuiState) -> IO ()
updateGuiStateLoop markupReader guiStateMVar = forever $ do
  json <- maybeReadPrinterJson markupReader
  postGUISync $ case json of
    Left error -> guiError error guiStateMVar
    Right gms -> do
      buildWidgets gms guiStateMVar
      updateWidgets guiStateMVar

getClickCommand :: (MVar GuiState) -> IO (Maybe String)
getClickCommand guiStateMVar = do
  guiState <- readMVar guiStateMVar
  return $ clickCommand guiState

jsonWidgetNew :: (IO String) -> IO Widget
jsonWidgetNew markupReader = do
  p <- hBoxNew False 0
  guiStateMVar <- newMVar $ GuiState { guiMarkups=[]
                                     , widgets=[]
                                     , panel=castToContainer p
                                     , clickCommand=Nothing
                                     }
  forkIO $ updateGuiStateLoop markupReader guiStateMVar
  clickableLeftAsync (getClickCommand guiStateMVar) p
