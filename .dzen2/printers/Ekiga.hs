module Ekiga(main) where
import Utils(isRunning)
import ClickAction(clickActions)

main = do
  running <- isRunning "ekiga"
  let text = if running then "e" else "-"
  putStr $ clickActions ["ekiga", "", "killall ekiga"] text
