module Image(imageW) where
import PixbufCache (pixbufCacheNew, loadPixbuf)
import Utils (defaultDelay)

import Graphics.UI.Gtk (
  imageNew, imageClear, imageSetFromPixbuf,
  realize, on, toWidget, postGUIAsync)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception as E (catch, IOException)

imageW cmd = do
  img <- imageNew
  pbCache <- pixbufCacheNew
  on img realize $ void $ forkIO $ forever $ do
    filepath <- cmd
    pixbuf <- loadPixbuf pbCache filepath
    postGUIAsync $ case pixbuf of
      Just pb -> imageSetFromPixbuf img pb
      Nothing -> imageClear img
    threadDelay $ floor (defaultDelay * 1000000)
  return img
