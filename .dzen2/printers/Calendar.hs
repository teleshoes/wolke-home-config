module Calendar(main) where
import System.Process(readProcess)
import Data.List (intercalate, intersperse, transpose)
import Data.List.Split (splitEvery, splitOn)
import Utils (fg, bg, padL, padR, estimateLength)
import Control.Arrow (first, second)

main = do
  [y,m,d] <- mapM runDate ["Y", "m", "d"]
  cals <- mapM (getCal (y,m,d)) [-23..24]
  putStr $ formatColumns cals 6

runCal (y,m) = readProcess "cal" ["-h", show m, show y] ""
runDate f = fmap read $ readProcess "date" ["+%" ++ f] "" :: IO Int

replaceAll old new list = intercalate new $ splitOn old list

getCal (y,m,d) monthOffset = fmap style $ runCal $ relMonth monthOffset (y,m)
  where style = if monthOffset == 0 then styleThisMonth d else id


relMonth n = fromAbsMonth . (+n) . toAbsMonth
  where toAbsMonth   = uncurry (+) . first (*12) . second (subtract 1)
        fromAbsMonth = second (+1) . (`quotRem` 12)

styleThisMonth d c = unlines $ (bg "blue" header):(map styleDate dateLines)
  where (header:dateLines) = lines c
        styleDate = replaceAll dateSq (bg "white" $ fg "black" dateSq)
        dateSq = padL ' ' 2 $ show d

formatColumns cals height = columns cols "|  "
  where cols = map (lines.concat) (splitEvery height cals)

columns :: [[String]] -> String -> String
columns cols sep = intercalate "\n" $ map concat $ map (intersperse sep) rows
  where rows = transpose paddedCols
        paddedCols = zipWith (\len col -> map (padR ' ' len) col) maxLens cols
        maxLens = map (maximum . (map estimateLength)) cols
