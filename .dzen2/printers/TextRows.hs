import System.Environment.UTF8 (getArgs)
import StripMarkup (estimateLength)

main = do
 args <- getArgs
 putStr $ textRows (read (args !! 2) :: Int) (args !! 0) (args !! 1)

offset height = 0 - (height `div` 12)

topPx height = offset height
botPx height = offset height + (height `div` 2)

posAbsY y m = "^pa(;" ++ (show y) ++ ")" ++ m

raise height m = posAbsY (topPx height) m
lower height m = posAbsY (botPx height) m

textRows height top bot = trm (raise height top) (lower height bot)
  where trm = if estimateLength top > estimateLength bot
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

