module ClickAction (main, clickAction, clickActions) where
import System.Environment.UTF8 (getArgs)
import Utils (clickArea, shiftUp, shiftMiddle)

main = do
  args <- getArgs
  let [btn,cmd,mrk] = case length args of
                      2 -> "1":args
                      3 -> args
                      _ -> error "Usage: <optional button> command markup"
  putStr $ clickAction (read btn :: Int) cmd mrk

clickActions cmds m = shiftUp ++ markup ++ shiftMiddle
  where markup = foldr ($) m (zipWith clickArea [1..] cmds)

clickAction btn cmd m = shiftUp ++ clickArea btn cmd m ++ shiftMiddle
