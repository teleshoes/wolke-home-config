module ClickAction (clickAction) where

clickAction button cmd m = ""
  ++ "^pa(;0)"
  ++ "^ca(" ++ button ++ "," ++ cmd ++ ")"
  ++ m
  ++ "^ca()"
  ++ "^pa()"
