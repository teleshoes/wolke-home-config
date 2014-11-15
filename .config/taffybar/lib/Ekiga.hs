module Ekiga(ekigaW) where
import Clickable (clickable)
import Label (labelW, mainLabel)
import Utils (isRunning)

main = mainLabel ekigaReader
ekigaW = clickable clickL clickM clickR =<< labelW ekigaReader

clickL = Just "ekiga"
clickM = Nothing
clickR = Just "pkill ekiga"

ekigaReader = do
  running <- isRunning "ekiga"
  return $ if running then "e" else "-"
