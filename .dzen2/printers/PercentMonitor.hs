#!/usr/bin/runghc
import Data.List (intercalate, stripPrefix, intersperse)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Char
import qualified Data.Foldable as Foldable (concatMap)
import Control.Monad
import Control.Applicative((<$>))
import Control.Exception
import System.Environment.UTF8 (getArgs)

main = do
  (w,h,colors) <- parseArgs <$> getArgs
  let loop i oldSamples = do
      samples <- (:(pop oldSamples)) . getPercents <$> getLine
      print $ formatSamples h samples
      loop ((i+1) `mod` w) samples
  loop 0 $ replicate w []

pop = reverse.tail.reverse

i = read :: String -> Int
f = read :: String -> Float


isInt   = maybe False id . fmap (null . snd) . listToMaybe .
          (reads :: ReadS Int)
isFloat = maybe False id . fmap (null . snd) . listToMaybe .
          (reads :: ReadS Float)


parseArgs (w:h:colors) | isInt w && isInt h = (i w, i h, colors)
parseArgs _ = error "Usage: width height color1 color2 [color3 color4 ...]\n"

getPercents line | isPer = map f $ pers
                 | otherwise = error ("Could not parse % line: " ++ line)
  where pers = words line
        isPer = all isFloat pers && length pers > 0

formatSamples :: Int -> [[Float]] -> String
formatSamples height samples = markup --"^ib(1)" ++ markup ++ "^ib(0)^fg()^pa()"
  where px = fmap (pixels height) (filter (not . null) samples)
        markup = Foldable.concatMap show px

drawRectStack width heights colors = addResets $ map (rect width) hcos
  where hcos = filter ((>0).first) $ zip3 heights colors (0:heights)
        addResets = intersperse ("^p(-" ++ (show width) ++ ")")
        first (x,_,_) = x


rect width (height,color,offset) = "^pa(;" ++ offset ++ ")" ++
                                   "^fg(" ++ color ++ ")" ++
                                   "^r(" ++ width ++ "x" ++ height ++ ")"

pixels :: Int -> [[Float]] -> [[Int]]
pixels totalHeight samplePers = ensureHeight totalHeight $ fmap px samplePers
  where px 0.0 = 0
        px per = if hpx per == 0 then 1 else hpx per
        hpx per = floor $ ((fromIntegral totalHeight)-2) * (per/100.0)
        ensureHeight h px | sum px > h-2 = ensureHeight h (decFirst h px)
                          | sum px < h-2 = ensureHeight h (incFirst h px)
                          | otherwise = px
        applyToFirstThat f b = (\(xs,(y:ys)) -> xs++(f y):ys) . break b
        decFirst h p = applyToFirstThat (-1) (>h+0) p
        incFirst h p = applyToFirstThat (+1) (<h-2) p

