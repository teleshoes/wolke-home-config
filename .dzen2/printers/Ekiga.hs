module Ekiga(main) where
import Utils(isRunning)
import ClickAction(clickAction)

main = do
  running <- isRunning "ekiga"
  
  putStr $
    clickAction "1" "ekiga" $
    clickAction "3" "killall ekiga" $
    if running then "e" else "-"

