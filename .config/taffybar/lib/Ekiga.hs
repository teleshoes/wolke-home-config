module Ekiga(ekigaW) where
import Widgets (clickable, label)
import Utils (isRunning)

getEkiga = do
  running <- isRunning "ekiga"
  return $ if running then "e" else "-"

ekigaW = do
  lbl <- label getEkiga
  click <- clickable lbl (Just "ekiga") Nothing (Just "pkill ekiga")
  return click
