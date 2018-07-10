module PixbufCache (pixbufCacheNew, loadPixbuf) where
import Utils (tryMaybe)
import qualified Data.Map as M (Map, empty, insert, lookup)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar, putMVar)
import GI.GdkPixbuf.Objects.Pixbuf (Pixbuf, pixbufNewFromFile)

type PixbufCache = MVar (M.Map String Pixbuf)

get :: PixbufCache -> String -> IO (Maybe Pixbuf)
get pbCache filepath = fmap (M.lookup filepath) $ readMVar pbCache

set :: PixbufCache -> String -> Pixbuf -> IO ()
set pbCache filepath pb = putMVar pbCache . M.insert filepath pb =<< takeMVar pbCache

setMaybe :: PixbufCache -> String -> (Maybe Pixbuf) -> IO ()
setMaybe pbCache filepath (Just pb) = set pbCache filepath pb
setMaybe _ _ Nothing = return ()


pixbufCacheNew :: IO PixbufCache
pixbufCacheNew = newMVar M.empty

loadPixbuf :: PixbufCache -> String -> IO (Maybe Pixbuf)
loadPixbuf pbCache filepath = do
  cachedPb <- get pbCache filepath
  case cachedPb of
    Just _  -> return cachedPb
    Nothing -> do
      pixbuf <- tryMaybe $ pixbufNewFromFile filepath
      setMaybe pbCache filepath pixbuf
      return pixbuf
