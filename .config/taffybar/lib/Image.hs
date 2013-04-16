module Image(imageW) where
import PixbufCache (pixbufCacheNew, loadPixbuf)
import Utils (defaultDelay)

import Graphics.UI.Gtk (
  imageNew, imageClear, imageSetFromPixbuf,
  realize, on, toWidget, postGUIAsync)
import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Exception as E (catch, IOException)

imageW cmd = do
  img <- imageNew
  pbCache <- pixbufCacheNew
  lastFileMVar <- newMVar ""
  on img realize $ void $ forkIO $ forever $ do
    filepath <- cmd
    prevFilepath <- takeMVar lastFileMVar
    unless (filepath == prevFilepath) $ do
      pixbuf <- loadPixbuf pbCache filepath
      postGUIAsync $ case pixbuf of
        Just pb -> imageSetFromPixbuf img pb
        Nothing -> imageClear img
    putMVar lastFileMVar filepath
    threadDelay $ floor (defaultDelay * 1000000)
  return img
