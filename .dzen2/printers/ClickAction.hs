module ClickAction (clickAction) where
import System.Environment.UTF8 (getArgs)

clickAction button cmd m = ""
  ++ "^pa(;0)"
  ++ "^ca(" ++ button ++ ", " ++ cmd ++ ")"
  ++ m
  ++ "^ca()"
  ++ "^pa()"
