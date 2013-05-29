module Ekiga(ekigaW) where
import Clickable (clickable)
import Label (labelW)
import Utils (isRunning)

clickL = Just "ekiga"
clickM = Nothing
clickR = Just "pkill ekiga"

getEkiga = do
  running <- isRunning "ekiga"
  return $ if running then "e" else "-"

ekigaW = clickable clickL clickM clickR =<< labelW getEkiga
