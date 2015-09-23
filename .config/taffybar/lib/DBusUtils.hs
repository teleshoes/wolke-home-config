{-# LANGUAGE OverloadedStrings #-}
module DBusUtils
 ( dbusConnect
 ) where
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import qualified DBus        as DBus
import qualified DBus.Client as DBus

import Utils

dbusConnect :: IO (Maybe DBus.Client)
dbusConnect = runMaybeT $ do
  address <- MaybeT DBus.getSessionAddress
         <|> MaybeT sessionAddressFromFile
  client <- lift $ DBus.connect address
  lift $ DBus.requestName client "user.taffybar" [DBus.nameAllowReplacement, DBus.nameReplaceExisting]
  return client
  where
    sessionAddressFromFile = (>>= DBus.parseAddress)
     . listToMaybe . catMaybes
     . map (stripPrefix "DBUS_SESSION_BUS_ADDRESS=")
     <$> systemReadLines "cat ~/.dbus/session-bus/*"
