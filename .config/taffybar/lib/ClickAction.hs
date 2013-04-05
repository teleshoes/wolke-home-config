module ClickAction (main, clickAction, clickActions) where
import System.Environment.UTF8 (getArgs)
import Utils (clickArea, shiftTop, shiftMid)

main = do
  args <- getArgs
  let [btn,cmd,mrk] = case length args of
                      2 -> "1":args
                      3 -> args
                      _ -> error "Usage: <optional button> command markup"
  putStr $ clickAction (read btn :: Int) cmd mrk

clickActions cmds m = shiftTop ++ markup ++ shiftMid
  where markup = foldr ($) m (zipWith clickArea [1..] cmds)

clickAction btn cmd m = shiftTop ++ clickArea btn cmd m ++ shiftMid
