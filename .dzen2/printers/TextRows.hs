module TextRows(textRows, main) where
import System.Environment.UTF8 (getArgs)
import StripMarkup (estimateLength)
import Utils (height)

topPx = 0 - (height `div` 12)
botPx = topPx + (height `div` 2)

main = do
 args <- getArgs
 putStr $ textRows (args !! 0) (args !! 1)

posAbsY y m = "^pa(;" ++ (show y) ++ ")" ++ m

textRows top bot = trm (posAbsY topPx top) (posAbsY botPx bot)
  where trm = if estimateLength top < estimateLength bot
              then textRowsMarkup
              else flip textRowsMarkup

textRowsMarkup shorter longer = ""
  ++ "^p(_LOCK_X)"
  ++ shorter
  ++ "^ib(1)"
  ++ "^p(_UNLOCK_X)"
  ++ longer
  ++ "^ib(0)"
  ++ "^pa()"

