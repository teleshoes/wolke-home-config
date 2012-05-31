module ClickAction (main, clickAction, clickActions) where
import System.Environment.UTF8 (getArgs)

main = do
  args <- getArgs
  let [btn,cmd,mrk] = case length args of
                      2 -> "1":args
                      3 -> args
                      _ -> error "Usage: <optional button> command markup"
  putStr $ clickAction (read btn :: Int) cmd mrk

clickActions cmds m = foldr ($) m (zipWith clickAction [1..] cmds)

clickAction :: Int -> String -> String -> String
clickAction btn cmd markup = if null cmd then markup else ""
  ++ "^pa(;0)"
  ++ "^ca(" ++ show btn ++ ", " ++ cmd ++ ")"
  ++ markup
  ++ "^ca()"
  ++ "^pa()"
