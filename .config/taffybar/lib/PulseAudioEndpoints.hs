{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
module PulseAudioEndpoints
  ( paSinkW
  , paSourceW
  , EPCode
  , EPName
  ) where
import Control.Applicative
import Control.Concurrent
import Data.Foldable (forM_)
import Control.Monad.IO.Class
import Control.Monad.Reader hiding (forM_)
import Data.Function
import Data.Maybe
import Data.List
import Data.List.Split
import Data.String
import Data.Tuple
import qualified DBus.Client as DBus
import qualified DBus.Internal.Types as DBusTypes
import GI.Gtk.Objects.Widget (Widget)
import Safe
import System.Taffybar.Widget.Generic.PollingLabel

import Clickable
import Utils
import qualified WrappedMVar as W

import qualified Data.Text as T

data Endpoint = EP
  { infoKey    :: String
  , listName   :: String
  , setCmd     :: String
  , moveCmd    :: String
  , streamName :: String
  , dbusName   :: String
  }

type EPId   = String
type EPCode = String
type EPName = String
data EPInfo = EPInfo
  { epId   :: Maybe EPId
  , epCode :: Maybe EPCode
  , epName :: EPName
  }

data St = St
  { isLocked :: Bool
  , wantedEP :: EPName
  } deriving Show
data Env = Env
  { ep      :: Endpoint
  , codeMap :: [(EPCode, EPName)]
  }
type PAM a = ReaderT Env IO a
type StVar = W.MVar (ReaderT Env IO) St

paEndpointSink, paEndpointSource :: Endpoint
paEndpointSink = EP
  { infoKey    = "Default Sink"
  , listName   = "sinks"
  , setCmd     = "set-default-sink"
  , moveCmd    = "move-sink-input"
  , streamName = "sink-inputs"
  , dbusName   = "user.taffybar.PulseAudioEndpoint.Sink"
  }
paEndpointSource = EP
  { infoKey    = "Default Source"
  , listName   = "sources"
  , setCmd     = "set-default-source"
  , moveCmd    = "move-source-output"
  , streamName = "source-outputs"
  , dbusName   = "user.taffybar.PulseAudioEndpoint.Source"
  }

paSinkW, paSourceW :: Maybe DBus.Client -> [(EPCode, EPName)] -> Maybe EPCode -> IO Widget
paSinkW   = paEndpointW paEndpointSink
paSourceW = paEndpointW paEndpointSource

paEndpointW :: Endpoint -> Maybe DBus.Client -> [(EPCode, EPName)] -> Maybe EPCode -> IO Widget
paEndpointW ep mdbc codeMap initLock = flip runReaderT Env{..} $ do
  -- Validate configuration.
  let codes = map fst codeMap
  when (any (regexMatch "[>&]") codes) $
    error "PulseAudioEndpoint: EPCodes may not contain [>&]"
  unless (maybe True (`elem` codes) initLock) $
    error "PulseAudioEndpoint: Locked EPCode must be in code map"
  -- Forking actions that run in PAM.
  let async = void . forkIO . flip runReaderT Env{..} . void
      atInterval n act = liftIO .  async . forever $ do
        act >> liftIO (threadDelay . floor $ n * 10^6)
  -- Create a var that will produce label strings on write
  -- and a callback to read those label strings.
  (stvar, nextLabel) <- hookLabel initLock
  -- Every second, ensure that Pulse is using the locked endpoint
  -- and update the label.
  atInterval 1 $ forceLockedEP stvar
  -- If given a dbus client, register dbus methods.
  forM_ mdbc $ registerDBus stvar
  -- After long struggle, a widget is born.
  let toggleLock = async $ toggleLockSync stvar -- left click action
      cycle      = async $ cycleSync stvar      -- right click action
  liftIO $ pollingLabelNew (T.pack "----") 0 (fmap T.pack nextLabel)
       >>= clickableActions toggleLock pass cycle

hookLabel :: Maybe EPCode -> PAM (StVar, IO String)
hookLabel initLock = do
  hiddenVar <- liftIO newEmptyMVar
  labelChan <- liftIO newChan

  let stvar = W.wrapPutMVar hiddenVar $ \st -> do
        label <- labelFormat st <$> paGetDefault <*> paGetList
        liftIO $ do
          putMVar hiddenVar st
          writeChan labelChan label

  Env{codeMap} <- ask
  defEP <- paGetDefault
  let isLocked = isJust initLock
      wantedEP = fromMaybe defEP $ flip lookup codeMap =<< initLock
  W.putMVar stvar St{..}

  return (stvar, readChan labelChan)

forceLockedEP :: StVar -> PAM ()
forceLockedEP = flip W.withMVar $ \St{isLocked, wantedEP} ->
  when isLocked $ paSetAll wantedEP

toggleLockSync :: StVar -> PAM Bool
toggleLockSync = flip W.modifyMVar $ \st@St{isLocked, wantedEP} -> do
  let nowLocked = not isLocked
  when nowLocked $ paSetAll wantedEP
  return (st{isLocked = nowLocked}, nowLocked)

cycleSync :: StVar -> PAM (Maybe EPName)
cycleSync = flip W.modifyMVar $ \st@St{isLocked, wantedEP} ->
  if isLocked
  then return (st, Nothing)
  else do
    next <- paGetNext wantedEP
    paSetAll next
    return (st{wantedEP = next}, Just next)

paGetList :: PAM [EPInfo]
paGetList = do
  Env{ep = EP{listName}, codeMap} <- ask
  idMap <- filter notMonitor . map projIdName . lines <$> cmd listName
  return $ [EPInfo (findId cdnm idMap) (Just cd) cdnm | (cd, cdnm) <- codeMap]
        ++ [EPInfo (Just id)           Nothing   idnm | (id, idnm) <- remCoded idMap codeMap]
  where
    cmd lname = pactl ["list", "short", lname]
    projIdName = (\(id:name:_) -> (id, name)) . words
    notMonitor = not . regexMatch "\\.monitor$" . snd

    findId cdnm = lookup cdnm . map swap
    remCoded = deleteFirstsBy ((==) `on` snd)

paGetDefault :: PAM EPName
paGetDefault = do
  Env{ep = EP{infoKey}} <- ask
  fromMaybe "" . (projDef infoKey <=< find (defLine infoKey)) . lines <$> cmd
  where
    cmd = pactl ["info"]
    projDef key = regexFirstGroup $ concat ["^",key,": (.*)"]
    defLine key = regexMatch      $ concat ["^",key,": "]

paGetNext :: EPName -> PAM EPName
paGetNext curr = do
  names <- epName <$$> paGetList
  return . fromMaybe (headDef "" names) $ do
    currIx <- findIndex (== curr) names
    names `atMay` (succ currIx `mod` length names)

paSetAll :: EPName -> PAM ()
paSetAll target = do
  Env{ep = EP{setCmd, moveCmd, streamName}} <- ask
  maybeId <- findEPId target <$> paGetList
  forM_ maybeId $ \tid -> do
    setDef setCmd tid
    streams streamName >>= mapM_ (moveStream moveCmd tid)
  where
    setDef scmd tid = pactl [scmd, tid]
    moveStream mcmd tid strm = pactl [mcmd, strm, tid]
    streams sname = map (head . words) . lines <$> pactl ["list", "short", sname]

format :: St -> EPName ->  [EPInfo] -> String
format St{isLocked, wantedEP} defEP infoList = concat
  [ (defEP /= wantedEP) ? (codeOf defEP ++ ">")
  , codeOf wantedEP
  , isLocked ? "&"
  ] where
  codeOf name = fromMaybe "?" $ findEPCode name infoList <|> findEPId name infoList
  cond ? str = if cond then str else ""

labelFormat :: St -> EPName -> [EPInfo] -> String
labelFormat st@St{isLocked, wantedEP} defEP = color . pad . format st defEP
  where
    color = if defEP == wantedEP then fg "green" else fg "red"
    pad   = if isLocked then padL ' ' 4 else padR ' ' 4 . padL ' ' 3

parse :: String -> PAM (Maybe St)
parse str = do
  infoList <- paGetList
  return $ do
    [defCode, wantedCode, lock] <- regexGroups "^(?:([^>&]+)>)?([^>&]+)(&)?$" str
    let isLocked = lock /= ""
    wantedEP <- findEPName wantedCode infoList
    when (defCode /= "") . void $ findEPName defCode infoList
    return St{..}

registerDBus :: StVar -> DBus.Client -> PAM ()
registerDBus stvar dbc = do
  env@Env{ep = EP{dbusName}} <- ask
  let run = flip runReaderT env
  let objPath = fromString . concatMap ('/':) . splitOn "." $ dbusName
  let ifaceName = DBusTypes.interfaceName_ dbusName
  let methods = [ DBus.autoMethod "ToggleLock" (run toggleLock :: IO Bool)
                , DBus.autoMethod "Cycle"      (run cycle      :: IO (Bool, String))
                , DBus.autoMethod "Get"        (run get        :: IO String)
                , DBus.autoMethod "Set"        (run . set      :: String -> IO Bool)
                , DBus.autoMethod "List"       (run list       :: IO String)
                ]
  let iface = DBus.defaultInterface { DBus.interfaceName = ifaceName
                                    , DBus.interfaceMethods = methods
                                    }
  liftIO $ DBus.export dbc objPath iface
  where
    toggleLock = toggleLockSync stvar
    cycle = maybe (False, "") ((,) True) <$> cycleSync stvar

    get = W.withMVar stvar $ \st -> format st <$> paGetDefault <*> paGetList
    set str = do
      stMay <- parse str
      forM_ stMay $ \st@St{wantedEP} ->
        W.modifyMVar_ stvar $ \_ -> do
          paSetAll wantedEP
          return st
      return $ isJust stMay

    list = uncols " " [Right ' ', Right ' '] . map formatInfoLine <$> paGetList
    formatInfoLine EPInfo{epId, epCode, epName} =
      [fromMaybe "" epCode, fromMaybe "" epId, epName]


-- Utility
pactl :: [String] -> PAM String
pactl = liftIO . readProc . ("pactl":)

pass :: Monad m => m ()
pass = return ()

infixl 4 <$$>
-- | Map inside two levels of functor.
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap

findEPId :: EPName -> [EPInfo] -> Maybe EPId
findEPId name infos = join $ epId <$> find ((==) name . epName) infos

findEPCode :: EPName -> [EPInfo] -> Maybe EPCode
findEPCode name infos = join $ epCode <$> find ((==) name . epName) infos

findEPName :: EPCode -> [EPInfo] -> Maybe EPName
findEPName code infos = epName <$> (find ((==) (Just code) . epCode) infos
                               <|>  find ((==) (Just code) . epId  ) infos)
