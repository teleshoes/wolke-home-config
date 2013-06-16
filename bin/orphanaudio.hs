#!/usr/bin/runghc
import Control.Applicative
import System.Environment
import System.FilePath
import System.Process
import qualified Data.Set as S

main = do
    home  <- getEnv "HOME"
    oggs  <- findExt "ogg"  $ home </> "Music"
    flacs <- findExt "flac" $ home </> "Flacs"
    putStrLn "Orphan oggs: "
    mapM_ putStrLn . S.toList $ S.difference oggs flacs
    putStrLn ""
    putStrLn "Orphan flacs: "
    mapM_ putStrLn . S.toList $ S.difference flacs oggs

findExt ext dir = S.fromList
    . map (joinPath . drop 4 . splitPath)
    . map dropExtension
    . filter ((== ("." ++ ext)) . takeExtension)
    . lines
    <$> readProcess "find" ["-L",dir,"-type","f"] ""

