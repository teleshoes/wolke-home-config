#!/home/wolke/.cabal/bin/runghc
import Prelude hiding (mapM)
import Control.Applicative (pure, (<$>))
import Control.Exception (bracket)
import Control.Monad (filterM, liftM2, (<=<))
import Data.List (inits, tails, maximumBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Traversable (mapM)
import Graphics.X11.Xlib (openDisplay, closeDisplay, getInputFocus, wM_NAME)
import Graphics.X11.Xlib.Extras (queryTree, getTextProperty, wcTextPropertyToTextList)
import System.Directory (doesDirectoryExist)
import System.Environment.UTF8 --(getEnv)
import System.FilePath ((</>))
import System.Posix.Process (executeFile)
-- requires cabal packages: X11, utf8-string
import Data.Maybe

main :: IO ()
main = flip (executeFile "term" True) Nothing
     =<< liftM2 (++)
            (return . ("-cd":) . pure =<< extractDir =<< findFocus)
            getArgs

findFocus :: IO String
findFocus = ff `catch` const (return "")
  where 
    ff = bracket (openDisplay "") closeDisplay $ \dsp -> do
            win <- fst <$> getInputFocus dsp
            txtp <- getTextProperty dsp win wM_NAME
            head <$> wcTextPropertyToTextList dsp txtp

extractDir :: String -> IO String
extractDir s
  | s == "Downloads" = expandTilde "~/Desktop"
  | otherwise = maybe (expandTilde "~") return
              <=< mapM expandTilde
              . listToMaybeWith (maximumBy $ comparing length)
              <=< filterM (doesDirectoryExist <=< expandTilde)
              . filter ((`elem` "/~") . head)
              . filter (/= "/") . sublists $ s

expandTilde :: String -> IO String
expandTilde ('~':xs) = getEnv "HOME" >>= return . (++xs)
expandTilde xs = return xs

sublists :: [a] -> [[a]]
sublists = concatMap (tail . inits) . init . tails

list :: b -> (a -> [a] -> b) -> [a] -> b
list d _ []     = d
list _ f (x:xs) = f x xs

listToMaybeWith :: ([a] -> b) -> [a] -> Maybe b
listToMaybeWith _ [] = Nothing
listToMaybeWith f xs = Just $ f xs

{-
-- This version worked under gnome
findFocus = bracket (openDisplay "") (closeDisplay) $ \dsp -> do
    (win, _) <- getInputFocus dsp
    (_, par, _) <- queryTree dsp win
    txtp <- getTextProperty dsp par wM_NAME
    head <$> wcTextPropertyToTextList dsp txtp
-}
