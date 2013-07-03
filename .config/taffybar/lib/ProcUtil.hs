module ProcUtil(
  readProcessWithExitCode'
) where
import System.Process (createProcess, proc, terminateProcess, waitForProcess)

import Prelude hiding (mapM)

import System.Process.Internals
import Control.Exception (SomeException, mask, try, onException, throwIO)
import Control.DeepSeq (rnf)
import System.IO.Error (mkIOError, ioeSetErrorString)
import System.Posix.Types
import System.Posix.Process (getProcessGroupIDOf)
import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad
import Foreign
import Foreign.C
import System.IO
import Data.Maybe
import System.Exit      ( ExitCode(..) )
import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )
import System.Posix.Signals


readProcessWithExitCode'
    :: FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input =
    mask $ \restore -> do
      (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args)
                                                   { std_in  = CreatePipe,
                                                     std_out = CreatePipe,
                                                     std_err = CreatePipe }
      flip onException
        (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        hSetBinaryMode outh True
        out <- hGetContents outh
        waitOut <- forkWait $ C.evaluate $ rnf out

        -- fork off a thread to start consuming stderr
        hSetBinaryMode errh True
        err <- hGetContents errh
        waitErr <- forkWait $ C.evaluate $ rnf err

        -- now write and flush any input
        let writeInput = do
              unless (null input) $ do
                hPutStr inh input
                hFlush inh
              hClose inh

        C.catch writeInput $ \e -> case e of
          IOError { ioe_type = ResourceVanished
                  , ioe_errno = Just ioe }
            | Errno ioe == ePIPE -> return ()
          _ -> throwIO e

        -- wait on the output
        waitOut
        waitErr

        hClose outh
        hClose errh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out, err)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

