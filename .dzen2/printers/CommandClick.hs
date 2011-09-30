module CommandClick (commandClick, imgDir) where
import System.Environment.UTF8 (getArgs)
import System.Environment (getEnv)
import ShiftUp (shiftUp)
import TextRows (textRows)
import ClickableImage (clickableImage)

main = do
 args <- getArgs
 home <- getEnv "HOME"
 putStr $ commandClick home (args !! 0)

imgDir home = home ++ "/.dzen2/icons/36x36/"

commandClick home exec = clickableImage [exec] img
  where img = imgDir home ++ exec ++ ".xpm"

lastPathElem p = reverse $ takeWhile (/='/') (reverse p)
