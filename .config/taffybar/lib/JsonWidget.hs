{-# LANGUAGE DeriveFunctor #-}
module JsonWidget(jsonWidgetNew) where
import Clickable (clickableLeftAsync)

import Data.Maybe (listToMaybe)
import Data.Text (pack)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, takeMVar, putMVar)
import Control.Exception (try, SomeException)
import System.Process (system)
import System.IO (stderr, hPutStrLn)

import Data.GI.Gtk.Threading (postGUISync)
import Data.GI.Base.ManagedPtr (
  unsafeCastTo)
import GI.Gtk.Enums (
  Orientation(OrientationHorizontal))
import GI.Gtk.Objects.Widget (
  IsWidget, Widget, toWidget, widgetShowAll)
import GI.Gtk.Objects.Container (
  Container,
  containerAdd, containerRemove, containerForeach, toContainer)
import GI.Gtk.Objects.Box (
  boxNew, boxSetHomogeneous)
import GI.Gtk.Objects.Label (
  Label(Label), labelNew, labelSetMarkup, toLabel)
import GI.Gtk.Objects.Image (
  Image(Image), imageNew, imageSetFromFile, toImage)
import Text.JSON (JSObject, Result(Ok, Error), decode, fromJSObject)

data GuiMarkupF a = GMClick a | GMLabel a | GMImage a
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
  "click" -> GMClick value
  "label" -> GMLabel value
  "image" -> GMImage value

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
  errorLabel <- labelNew $ Just $ pack error
  errorLabelWidget <- toWidget errorLabel
  containerAdd p errorLabel
  widgetShowAll p
  putMVar guiStateMVar guiState { guiMarkups = []
                                , widgets = [errorLabelWidget]
                                , clickCommand = Nothing
                                }

parseClickCommand :: [GuiMarkup] -> (Maybe String, [GuiMarkup])
parseClickCommand gms = (cmd, widgetGuiMarkups)
  where clickGuiMarkup = listToMaybe $ filter isClickCmd gms
        cmd = case clickGuiMarkup of
          Nothing -> Nothing
          Just (GMClick cmd) -> Just cmd
        widgetGuiMarkups = filter (not . isClickCmd) gms
        isClickCmd x = case x of
          (GMClick cmd) -> True
          otherwise   -> False

buildWidget :: GuiMarkup -> IO Widget
buildWidget gm = case gm of
  (GMLabel text) -> toWidget =<< labelNew Nothing
  (GMImage img) -> toWidget =<< imageNew

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
  (GMLabel text) -> do
    lblW <- unsafeCastTo Label w
    labelSetMarkup lblW $ pack text
  (GMImage image) -> do
    imgW <- unsafeCastTo Image w
    imageSetFromFile imgW $ Just image

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
  p <- boxNew OrientationHorizontal 0
  boxSetHomogeneous p False
  containerPanel <- toContainer p
  guiStateMVar <- newMVar $ GuiState { guiMarkups = []
                                     , widgets = []
                                     , panel = containerPanel
                                     , clickCommand = Nothing
                                     }
  forkIO $ updateGuiStateLoop markupReader guiStateMVar
  clickableLeftAsync (getClickCommand guiStateMVar) p
