module Image(imageW) where
import PixbufCache (pixbufCacheNew, loadPixbuf)
import Utils (defaultDelay)

import Data.Maybe (isJust)

import Data.GI.Gtk.Threading (postGUIASync)
import GI.Gtk.Objects.Image (
  Image, imageNew, imageClear, imageSetFromPixbuf)
import GI.Gtk.Objects.Widget (onWidgetRealize)

import Control.Monad (forever, unless, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Exception as E (catch, IOException)

imageW :: (IO String) -> IO Image
imageW cmd = do
  img <- imageNew
  pbCache <- pixbufCacheNew
  lastFileMVar <- newMVar ""
  onWidgetRealize img $ void $ forkIO $ forever $ do
    filepath <- cmd
    prevFilepath <- takeMVar lastFileMVar
    unless (filepath == prevFilepath) $ do
      pixbuf <- loadPixbuf pbCache filepath
      postGUIASync $ if isJust pixbuf
                     then imageSetFromPixbuf img pixbuf
                     else imageClear img
    putMVar lastFileMVar filepath
    threadDelay $ floor (defaultDelay * 1000000)
  return img
