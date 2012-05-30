module ClickAction (main, clickAction, clickActionSet) where
import System.Environment.UTF8 (getArgs)

main = do
  args <- getArgs
  let [btn,cmd,mrk] = case length args of
                      2 -> "1":args
                      3 -> args
                      _ -> error "Usage: <optional button> command markup"
  putStr $ clickAction btn cmd mrk

clickAction btn cmd markup = ""
  ++ "^pa(;0)"
  ++ "^ca(" ++ btn ++ ", " ++ cmd ++ ")"
  ++ markup
  ++ "^ca()"
  ++ "^pa()"

clickActionSet btn1Cmd btn2Cmd btn3Cmd markup = id
  $ clickAction "1" btn1Cmd
  $ clickAction "2" btn2Cmd
  $ clickAction "3" btn3Cmd
  markup
