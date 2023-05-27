module Utils (
  (<$$>),
  relToHomeDir,
  readMachineType)
where

import Control.Exception (catch, IOException)
import Data.String.Utils (strip)
import System.FilePath ((</>))
import System.Directory (getHomeDirectory)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

readMachineType :: IO (Maybe String)
readMachineType = strip <$$> (maybeReadFile =<< relToHomeDir "machine-type")

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile file = (Just <$> readFile file) `catch` handler
   where handler :: IOException -> IO (Maybe String)
         handler e = do
           putStrLn $ "WARNING: could not read " ++ file ++ "\n" ++ show e
           return Nothing

relToHomeDir :: String -> IO String
relToHomeDir file = (</> file) <$> getHomeDirectory
