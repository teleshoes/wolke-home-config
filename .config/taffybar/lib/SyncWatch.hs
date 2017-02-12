module SyncWatch (syncWatchW, getSyncWatch, main) where
import JsonWidget (jsonWidgetNew)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Maybe (catMaybes, listToMaybe)
import Utils (readInt, regexFirstGroup, padL)

main = forever $ print =<< getSyncWatch

syncWatchW = jsonWidgetNew $ getSyncWatch

getSyncWatch :: IO String
getSyncWatch = do
  threadDelay $ 800 * 10^3

  let fields = ["Dirty", "Writeback"]
  meminfoBytes <- getMemInfoBytes fields

  let meminfoSizeFmts = map formatSize meminfoBytes

  let fmtLines = map formatLine $ zip fields meminfoSizeFmts
  let json = "{\"label\": \"" ++ intercalate "\\n" fmtLines ++ "\"}"

  return json

formatLine :: (String, String) -> String
formatLine (field,sizeFmt) = (field !! 0):sizeFmt

formatSize :: Maybe Integer -> String
formatSize Nothing = "?"
formatSize (Just sizeKiB) | gb > 0 = pad $ show gb ++ "G"
                          | otherwise = pad $ show mb
  where b = sizeKiB * 1024
        mb = b `div` 10^6
        gb = b `div` 10^9
        pad s = padL '_' 3 s

getMemInfoBytes :: [String] -> IO [Maybe Integer]
getMemInfoBytes fields = do
  meminfoLines <- fmap lines $ readFile "/proc/meminfo"
  return $ map (getMeminfoKiB meminfoLines) fields

getMeminfoKiB :: [String] -> String -> Maybe Integer
getMeminfoKiB meminfoLines field = listToMaybe fieldSizes
  where re = "^" ++ field ++ ":\\s+(\\d+) kB" :: String
        reGroups = map (regexFirstGroup re) meminfoLines :: [Maybe String]
        fieldSizes = catMaybes $ map readInt $ catMaybes $ reGroups :: [Integer]
