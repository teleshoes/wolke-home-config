module ClickAction (main, clickAction) where
import System.Environment.UTF8 (getArgs)

main = do
  args <- getArgs
  let [btn,cmd,mrk] = case length args of
                      2 -> "1":args
                      3 -> args
                      _ -> error "Usage: <optional button> command markup"
  putStr $ clickAction btn cmd mrk

clickAction btn cmd mrk = ""
  ++ "^pa(;0)"
  ++ "^ca(" ++ btn ++ ", " ++ cmd ++ ")"
  ++ mrk
  ++ "^ca()"
  ++ "^pa()"
