#!/usr/bin/runghc
import System.Environment.UTF8 (getArgs)
import ClickAction (clickAction)

main = do
  args <- getArgs
  putStr $ runClickAction args
  
runClickAction args
  | length args == 3 = clickAction (args !! 0) (args !! 1) (args !! 2)
  | length args == 2 = clickAction "1" (args !! 0) (args !! 1)

