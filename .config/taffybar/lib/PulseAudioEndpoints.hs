{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module PulseAudioEndpoints
  ( paSinkW
  , paSourceW
  ) where
import Control.Applicative
import Control.Concurrent
import Control.Monad
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

withDelay :: IO a -> IO ()
withDelay act = act >> threadDelay (1 * 10^6)

data Endpoint = Endpoint{infoKey, listName, setCmd, moveCmd, objListName, dbusName :: String}
data LockState = Locked | Unlocked
  deriving Show

paSinkW, paSourceW :: Maybe DBus.Client -> ([String] -> Maybe String) -> IO Widget
paSinkW   = paEndpointW Endpoint
  { infoKey     = "Default Sink"
  , listName    = "sinks"
  , setCmd      = "set-default-sink"
  , moveCmd     = "move-sink-input"
  , objListName = "sink-inputs"
  , dbusName    = "user.taffybar.PulseAudioEndpoint.Sink"
  }
paSourceW = paEndpointW Endpoint
  { infoKey     = "Default Source"
  , listName    = "sources"
  , setCmd      = "set-default-source"
  , moveCmd     = "move-source-output"
  , objListName = "source-outputs"
  , dbusName    = "user.taffybar.PulseAudioEndpoint.Source"
  }

paEndpointW :: Endpoint -> Maybe DBus.Client -> ([String] -> Maybe String) -> IO Widget
paEndpointW ep mdbc start = do
  -- start chooses an initial lock state
  names <- fmap snd <$> pactlList ep
  lockVar <- newMVar $ start names
  -- When lock is on, reset to stored endpoint every $delay.
  async . forever . withDelay $
    maybe pass (pactlSet ep) =<< readMVar lockVar
  -- Start the reader loop. The var can be read through dbus as well as by
  -- the label.
  readAct <- pactlReader ep lockVar
  -- If given a dbus client, register dbus methods.
  flip (maybe pass) mdbc $ registerDBus ep lockVar readAct
  -- Left click toggles the lock state.
  let toggleLock = async $ toggleLockSync ep lockVar
  -- Right click cycles the endpoint if unlocked.
  let cycle      = async $ cycleSync      ep lockVar
  -- After long struggle, a widget is born.
  clickableActions toggleLock pass cycle =<< labelW (labelFormat <$> readAct)

toggleLockSync :: Endpoint -> MVar (Maybe String) -> IO LockState
toggleLockSync ep lockVar = modifyMVar lockVar $ \case
  Nothing -> (\nm -> (Just nm, Locked  )) <$> pactlGet ep
  Just _  -> return  (Nothing, Unlocked)

cycleSync :: Endpoint -> MVar (Maybe String) -> IO Bool
cycleSync ep lockVar = withMVar lockVar $ \lk -> do
  when (isNothing lk) $
    pactlGet ep >>= pactlNext ep >>= pactlSet ep
  return $ isNothing lk

-- List endpoint IDs and NAMEs.
pactlList :: Endpoint -> IO [(String, String)]
pactlList ep = filter prop . map project . lines <$> cmd
  where
    cmd     = pactl ["list","short",listName ep]
    project = (\(id:name:_) -> (id, name)) . words
    prop    = not . regexMatch "\\.monitor$" . snd

-- Read the default endpoint.
pactlGet :: Endpoint -> IO String
pactlGet ep = fromMaybe "" . (project <=< find prop) . lines <$> cmd
  where
    cmd     = pactl ["info"]
    project = regexFirstGroup ("^" ++ infoKey ep ++ ": (.*)")
    prop    = regexMatch      ("^" ++ infoKey ep ++ ": ")

-- Read the endpoint after the default endpoint.
pactlNext :: Endpoint -> String -> IO String
pactlNext ep name = do
  names <- map snd <$> pactlList ep
  return . fromMaybe "" $ do
    curr <- findIndex (== name) names
    atMay names $ succ curr `mod` length names

-- Set the default endpoit and move all existing streams to it.
pactlSet :: Endpoint -> String -> IO ()
pactlSet ep name = maybe pass (\i -> setDef i >> moveEx i) =<< findId
  where
    findId   = fmap fst . find ((== name) . snd) <$> pactlList ep
    setDef i = pactl [setCmd ep,i]
    moveEx i = objs >>= mapM_ (\o -> pactl [moveCmd ep,o,i])
    objs     = map (head . words) . lines <$> pactl ["list","short",objListName ep]

pactlReader :: Endpoint -> MVar (Maybe String) -> IO (IO (String, Maybe String))
pactlReader ep lockVar = do
  outVar <- newMVar ("?", Nothing)
  async . forever . withDelay $ do
    swapMVar outVar =<< (,) <$> pactlGet ep <*> readMVar lockVar
  return $ readMVar outVar

format :: (String, Maybe String) -> String
format (s, lk) = case lk of
  Nothing            -> codeOf s
  Just l | s == l    -> ">" ++ codeOf s
         | otherwise -> codeOf s ++ ">" ++ codeOf l

labelFormat :: (String, Maybe String) -> String
labelFormat (s, lk) = color . padL ' ' 3 $ format (s, lk)
  where
    color = case lk of
      Nothing            -> fg "green"
      Just l | s == l    -> fg "green"
             | otherwise -> fg "red"

codeOf :: String -> String
codeOf name
  | regexMatch "pci.*analog" name = "B"
  | regexMatch "hdmi"        name = "H"
  | regexMatch "usb"         name = "U"
  | otherwise                     = "?"

nameOf :: [String] -> String -> Maybe String
nameOf names code = case code of
  "B" -> find (regexMatch "pci.*analog") names
  "H" -> find (regexMatch "hdmi"       ) names
  "U" -> find (regexMatch "usb"        ) names
  _   -> Nothing

parse :: Endpoint -> String -> IO (Maybe (String, Maybe String))
parse ep str = do
  names <- fmap snd <$> pactlList ep
  return $ case splitOn ">" str of
    [s]     -> nameOf names s >>= \name  -> return (name, Nothing)
    ["",s]  -> nameOf names s >>= \name  -> return (name, Just name)
    [s ,l]  -> nameOf names s >>= \sname ->
               nameOf names l >>= \lname -> return (sname, Just lname)
    _       -> mzero

registerDBus :: Endpoint -> MVar (Maybe String) -> IO (String, Maybe String) -> DBus.Client -> IO ()
registerDBus ep lockVar readAct dbc = DBus.export dbc objPath
    [ DBus.autoMethod ifaceName "Get"        (get        :: IO String)
    , DBus.autoMethod ifaceName "Set"        (set        :: String -> IO Bool)
    , DBus.autoMethod ifaceName "ToggleLock" (toggleLock :: IO String)
    , DBus.autoMethod ifaceName "Cycle"      (cycle      :: IO Bool)
    ]
  where
    objPath   = fromString . concatMap ('/':) . splitOn "." $ dbusName ep
    ifaceName = fromString $ dbusName ep

    get = format <$> readAct
    set str = do
      stMay <- parse ep str
      maybe pass (\(s, lk) -> swapMVar lockVar lk >> pactlSet ep s) stMay
      return $ isJust stMay
    toggleLock = show <$> toggleLockSync ep lockVar
    cycle      =          cycleSync      ep lockVar

-- Utility
async :: IO a -> IO ()
async = void . forkIO . void

pass :: IO ()
pass = return ()

pactl :: [String] -> IO String
pactl = readProc . ("pactl":)
