module PulseAudioEndpoints
  ( paSinkW
  , paSourceW
  ) where
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Graphics.UI.Gtk (Widget)
import Safe

import Clickable
import Label
import Utils

wait = threadDelay $ 1 * 10^6

data Endpoint = Endpoint{infoKey, listName, setCmd, moveCmd, moveObjListName :: String}

paSinkW, paSourceW :: ([String] -> Maybe String) -> IO Widget
paSinkW   = paEndpointW Endpoint
  { infoKey         = "Default Sink"
  , listName        = "sinks"
  , setCmd          = "set-default-sink"
  , moveCmd         = "move-sink-input"
  , moveObjListName = "sink-inputs"
  }
paSourceW = paEndpointW Endpoint
  { infoKey         = "Default Source"
  , listName        = "sources"
  , setCmd          = "set-default-source"
  , moveCmd         = "move-source-output"
  , moveObjListName = "source-outputs"
  }

paEndpointW :: Endpoint -> ([String] -> Maybe String) -> IO Widget
paEndpointW ep start = do
  names <- fmap snd <$> pactlList ep
  forceVar <- newMVar $ start names
  let cycle       = async $ pactlGet ep >>= pactlNext ep >>= pactlSet ep
      toggleForce = async $ modifyMVar_ forceVar $ \m -> case m of
        Nothing -> Just <$> pactlGet ep
        Just _  -> return Nothing

  async . forever $ do
    maybe pass (pactlSet ep) =<< readMVar forceVar
    wait

  clickableActions cycle pass toggleForce
    =<< labelW =<< pactlReader ep forceVar

async :: IO a -> IO ()
async = void . forkIO . void

pass :: IO ()
pass = return ()

pactlList :: Endpoint -> IO [(String, String)]
pactlList ep
  = filter (not . (".monitor" `isSuffixOf`) . snd)
  . map ((\(ix:name:_) -> (ix,name)) . words)
  . lines <$> readProc ["pactl","list","short",listName ep]

pactlGet :: Endpoint -> IO String
pactlGet ep
  = fromMaybe ""
  . fmap (dropWhile isSpace . drop 1 . snd)
  . find ((== infoKey ep) . fst)
  . map (break (== ':'))
  . lines <$> readProc ["pactl","info"]

pactlNext :: Endpoint -> String -> IO String
pactlNext ep sink = do
  names <- map snd <$> pactlList ep
  let curr = fromMaybe 0 $ findIndex (== sink) names
  return $ atDef "" names (succ curr `mod` length names)

pactlSet :: Endpoint -> String -> IO ()
pactlSet ep name
  = maybe pass set
  . fmap fst . find ((== name) . snd)
  =<< pactlList ep
  where
    set s = do
      readProc ["pactl",setCmd ep,s]
      mapM_ (\input -> readProc ["pactl",moveCmd ep,input,s])
        =<< ( map (head . words)
            . lines <$> readProc ["pactl","list","short",moveObjListName ep])

pactlReader :: Endpoint -> MVar (Maybe String) -> IO (IO String)
pactlReader ep forceVar = do
  outVar <- newMVar "?"
  async . forever $ do
    s     <- pactlGet ep
    force <- readMVar forceVar
    let desc name | "usb"  `isInfixOf` name = "U"
                  | "hdmi" `isInfixOf` name = "H"
                  | "pci"  `isInfixOf` name = "B"
                  | otherwise               = "?"
    swapMVar outVar $ case force of
      Nothing ->                fg "green" $ desc s ++ "  "
      Just f  -> if s == f then fg "green" $ desc s ++ ">" ++ desc f
                           else fg "red"   $ desc s ++ ">" ++ desc f
    wait
  return $ readMVar outVar
