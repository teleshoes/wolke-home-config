module ClickAction (clickAction, clickActions) where
import System.Environment.UTF8 (getArgs)
import Utils (clickArea, shiftTop, shiftMid)

clickActions cmds m = m
clickAction btn cmd m = m


clickActions' cmds m = shiftTop ++ markup ++ shiftMid
  where markup = foldr ($) m (zipWith clickArea [1..] cmds)

clickAction' btn cmd m = shiftTop ++ clickArea btn cmd m ++ shiftMid
