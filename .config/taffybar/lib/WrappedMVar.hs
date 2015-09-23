{-# LANGUAGE RankNTypes, RecordWildCards #-}
module WrappedMVar
  ( MVar(..)
  , wrapMVar
  , wrapTakeMVar
  , wrapPutMVar
  , wrapMVar_
  ) where
import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Catch
import Control.Monad.IO.Class

data MVar m a = MVar
  { takeMVar          :: m a
  , putMVar           :: a -> m ()
  , readMVar          :: m a
  , tryTakeMVar       :: m (Maybe a)
  , tryPutMVar        :: a -> m Bool
  , tryReadMVar       :: m (Maybe a)
  , isEmptyMVar       :: m Bool
  , swapMVar          :: a -> m a
  , withMVar          :: forall b. (a -> m b) -> m b
  , withMVarMasked    :: forall b. (a -> m b) -> m b
  , modifyMVar_       :: (a -> m a) -> m ()
  , modifyMVar        :: forall b. (a -> m (a, b)) -> m b
  , modifyMVarMasked_ :: (a -> m a) -> m ()
  , modifyMVarMasked  :: forall b. (a -> m (a, b)) -> m b
  }

-- | The MVar API has 7 primitive functions:
-- 'MVar.takeMVar', 'MVar.putMVar', 'MVar.readMVar',
-- 'MVar.tryTakeMVar', 'MVar.tryPutMVar', 'MVar.tryReadMVar',
-- 'MVar.isEmptyMVar'
--
-- And also 7 composite functions defined in terms of 'MVar.takeMVar' and 'MVar.putMVar':
-- 'MVar.swapMVar', 'MVar.withMVar', 'MVar.withMVarMasked',
-- 'MVar.modifyMVar_', 'MVar.modifyMVar', 'MVar.modifyMVarMasked_', 'MVar.modifyMVarMasked'
--
-- wrapMVar creates a structure populated with lifted versions of the primitive functions
-- and composite functions based on the takeMVar and putMVar supplied.
--
-- Example of a wrapped MVar that logs every take and put:
--
-- @
--     import Control.Concurrent.MVar
--     import qualified WrappedMVar as W
--     import Control.Monad.IO.Class
--     import Control.Monad.Writer
--
--     newEmptyLoggingMVar = do
--       m <- liftIO newEmptyMVar
--       let take = do
--             a <- liftIO $ takeMVar m
--             tell [Right a]
--             return a
--           put a = do
--             liftIO $ putMVar m a
--             tell [Left a]
--           tryTake = do
--             ma <- liftIO $ tryTakeMVar m
--             maybe (return ()) (\a -> tell [Right a]) ma
--             return ma
--           tryPut a = do
--             b <- liftIO $ tryPutMVar m a
--             when b $ tell [Left a]
--             return b
--       return $ (W.wrapMVar m take put){W.tryTakeMVar = tryTake, W.tryPutMVar = tryPut}
-- @
wrapMVar :: (MonadIO m, MonadMask m, Monad m)
         => MVar.MVar a -- ^ The MVar to wrap.
         -> m a         -- ^ The new takeMVar.
         -> (a -> m ()) -- ^ The new putMVar.
         -> MVar m a
wrapMVar mvar takeMVar putMVar = MVar{..} where
  readMVar    = liftIO $ MVar.readMVar    mvar
  tryTakeMVar = liftIO $ MVar.tryTakeMVar mvar
  tryPutMVar  = liftIO . MVar.tryPutMVar  mvar
  tryReadMVar = liftIO $ MVar.tryReadMVar mvar
  isEmptyMVar = liftIO $ MVar.isEmptyMVar mvar
  swapMVar new =
    mask_ $ do
      old <- takeMVar
      putMVar new
      return old
  withMVar io =
    mask $ \restore -> do
      a <- takeMVar
      b <- restore (io a) `onException` putMVar a
      putMVar a
      return b
  withMVarMasked io =
    mask_ $ do
      a <- takeMVar
      b <- io a `onException` putMVar a
      putMVar a
      return b
  modifyMVar_ io =
    mask $ \restore -> do
      a  <- takeMVar
      a' <- restore (io a) `onException` putMVar a
      putMVar a'
  modifyMVar io =
    mask $ \restore -> do
      a      <- takeMVar
      (a',b) <- restore (io a >>= evaluate) `onException` putMVar a
      putMVar a'
      return b
  modifyMVarMasked_ io =
    mask_ $ do
      a  <- takeMVar
      a' <- io a `onException` putMVar a
      putMVar a'
  modifyMVarMasked io =
    mask_ $ do
      a      <- takeMVar
      (a',b) <- (io a >>= evaluate) `onException` putMVar a
      putMVar a'
      return b

  evaluate x = (return $! x) >>= return

-- | Like wrapMVar but using a default putMVar.
wrapTakeMVar :: (MonadIO m, MonadMask m, Monad m)
             => MVar.MVar a -- ^ The MVar to wrap.
             -> m a         -- ^ The new takeMVar.
             -> MVar m a
wrapTakeMVar mvar take = wrapMVar mvar take put where
  put = liftIO . MVar.putMVar  mvar

-- | Like wrapMVar but using a default takeMVar.
wrapPutMVar :: (MonadIO m, MonadMask m, Monad m)
            => MVar.MVar a -- ^ The MVar to wrap.
            -> (a -> m ()) -- ^ The new putMVar.
            -> MVar m a
wrapPutMVar mvar put = wrapMVar mvar take put where
  take = liftIO $ MVar.takeMVar mvar

-- | Like wrapMVar but using both default takeMVar and putMVar.
wrapMVar_ :: (MonadIO m, MonadMask m, Monad m)
            => MVar.MVar a -- ^ The MVar to wrap.
            -> MVar m a
wrapMVar_ mvar = wrapMVar mvar take put where
  take = liftIO $ MVar.takeMVar mvar
  put  = liftIO . MVar.putMVar  mvar
