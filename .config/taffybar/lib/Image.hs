module Image(imageW) where
import Utils (defaultDelay)

import Graphics.UI.Gtk (
  imageNew, imageNewFromFile, imageSetFromFile,
  realize, on, toWidget, postGUIAsync)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception as E (catch, IOException)

image file = do
  img <- imageNewFromFile file
  return $ toWidget img

imageW cmd = do
  img <- imageNew
  on img realize $ do
    forkIO $ forever $ do
      let tryUpdate = do
            file <- cmd
            postGUIAsync $ imageSetFromFile img file
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (defaultDelay * 1000000)
    return ()
  return img

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()
