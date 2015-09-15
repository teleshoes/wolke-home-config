{-# LANGUAGE OverloadedStrings #-}
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
import Data.List.Split
import Data.String
import qualified DBus.Client as DBus
import Graphics.UI.Gtk (Widget)
import Safe

import Clickable
import Label
import Utils

wait = threadDelay $ 1 * 10^6

data Endpoint = Endpoint{infoKey, listName, setCmd, moveCmd, moveObjListName, dbusName :: String}

paSinkW, paSourceW :: Maybe DBus.Client -> ([String] -> Maybe String) -> IO Widget
paSinkW   = paEndpointW Endpoint
  { infoKey         = "Default Sink"
  , listName        = "sinks"
  , setCmd          = "set-default-sink"
  , moveCmd         = "move-sink-input"
  , moveObjListName = "sink-inputs"
  , dbusName        = "user.taffybar.PulseAudioEndpoint.Sink"
  }
paSourceW = paEndpointW Endpoint
  { infoKey         = "Default Source"
  , listName        = "sources"
  , setCmd          = "set-default-source"
  , moveCmd         = "move-source-output"
  , moveObjListName = "source-outputs"
  , dbusName        = "user.taffybar.PulseAudioEndpoint.Source"
  }

paEndpointW :: Endpoint -> Maybe DBus.Client -> ([String] -> Maybe String) -> IO Widget
paEndpointW ep mdbc start = do
  names <- fmap snd <$> pactlList ep
  forceVar <- newMVar $ start names

  async . forever $ do
    maybe pass (pactlSet ep) =<< readMVar forceVar
    wait

  readAct <- pactlReader ep forceVar

  flip (maybe pass) mdbc $ registerDBus ep forceVar readAct

  let cycle = async $ do
        next <- pactlNext ep =<< pactlGet ep
        modifyMVar_ forceVar (return . (next <$))
        pactlSet ep next
      toggleForce = async $ modifyMVar_ forceVar $ \m -> case m of
        Nothing -> Just <$> pactlGet ep
        Just _  -> return Nothing
  clickableActions cycle pass toggleForce =<< labelW readAct

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
  = maybe pass (\s -> do
      readProc ["pactl",setCmd ep,s]
      objs <- map (head . words) . lines <$> readProc ["pactl","list","short",moveObjListName ep]
      forM_ objs $ \o -> readProc ["pactl",moveCmd ep,o,s])
  . fmap fst . find ((== name) . snd)
  =<< pactlList ep

pactlReader :: Endpoint -> MVar (Maybe String) -> IO (IO String)
pactlReader ep forceVar = do
  outVar <- newMVar "?"
  async . forever $ do
    s     <- pactlGet ep
    force <- readMVar forceVar
    swapMVar outVar $ case force of
      Nothing ->                fg "green" [codeOf s,' '     ,' '     ]
      Just f  -> if s == f then fg "green" ['>'     ,codeOf s,' '     ]
                           else fg "red"   [codeOf s,'>'     ,codeOf f]
    wait
  return $ readMVar outVar

codeOf :: String -> Char
codeOf name
  | "usb"  `isInfixOf` name = 'U'
  | "hdmi" `isInfixOf` name = 'H'
  | "pci"  `isInfixOf` name = 'B'
  | otherwise               = '?'

nameOf :: [String] -> Char -> Maybe String
nameOf names code = case code of
  'U' -> find ("usb"  `isInfixOf`) names
  'H' -> find ("hdmi" `isInfixOf`) names
  'B' -> headDef Nothing $ map Just names \\ [nameOf names 'U', nameOf names 'H']
  _   -> Nothing

registerDBus :: Endpoint -> MVar (Maybe String) -> IO String -> DBus.Client -> IO ()
registerDBus ep forceVar readAct dbc = DBus.export dbc objPath
    [ DBus.autoMethod ifaceName "Get" (get :: IO String)
    , DBus.autoMethod ifaceName "Set" (set :: String -> IO Bool)
    ]
  where
    objPath   = fromString . concatMap ('/':) . splitOn "." $ dbusName ep
    ifaceName = fromString $ dbusName ep

    get = takeWhile (/= '<') . tailSafe . dropWhile (/= '>') <$> readAct

    set str = do
      names <- map snd <$> pactlList ep
      let parse = case filter (not . isSpace) str of
            [code]       -> Just (code, False)
            ['>',code]   -> Just (code, True )
            [_,'>',code] -> Just (code, True )
            _            -> Nothing
      case parse >>= \(code, force) -> (,) force <$> nameOf names code of
        Just (force, name) -> do
          swapMVar forceVar $ if force then Just name else Nothing
          pactlSet ep name
          return True
        Nothing   -> do
          return False
