module ClickableImage (main, clickableImage) where
import System.Environment.UTF8 (getArgs)
import System.Process(readProcess)
import ClickAction (clickAction)
import Utils (shiftUp)

main = do
 args <- getArgs
 putStr $ shiftUp $ uncurry clickableImage $ parseArgs args

parseArgs args | length args == 4 = (take 3 args, args !! 3)
               | length args == 3 = (take 2 args, args !! 2)
               | length args == 2 = (take 1 args, args !! 1)
               | otherwise = error "Usage: btn1Cmd [btn2Cmd] [btn3Cmd] imgpath"

clickableImage cmds image = wrapCmds 1 cmds $ "^i(" ++ image ++ ")"
wrapCmds cnt (c:cs) m = wrapCmds (cnt+1) cs (clickAction (show cnt) c m)
wrapCmds _ [] m = m

