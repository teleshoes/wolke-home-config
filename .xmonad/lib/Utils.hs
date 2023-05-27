module Utils (
  relToHomeDir)
where

import System.FilePath ((</>))
import System.Directory (getHomeDirectory)

relToHomeDir :: String -> IO String
relToHomeDir file = (</> file) <$> getHomeDirectory
