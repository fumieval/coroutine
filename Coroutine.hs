
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Coroutine where

import GHC.Prim
import GHC.IO (IO(..))
import Data.Foldable
import Control.Monad.IO.Class
import Data.Functor
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad
import Control.Monad.Trans.Reader
import System.IO.Unsafe

unsafeReset :: IO a -> IO a
unsafeReset (IO a) = IO (reset# a)

unsafeShift :: ((IO a -> IO b) -> IO b) -> IO a
unsafeShift f = IO $ shift# $ \k -> case f (\(IO a) -> IO (k a)) of IO b -> b

newtype Source m a = Source { unSource :: m (Either (a, Source m a) ()) }

emptySource :: Applicative m => Source m a
emptySource = Source $ pure $ Right ()

type SourceBuilder a = IORef (Source IO a)

loremIpsum :: ReaderT (SourceBuilder String) IO ()
loremIpsum = for_ (zip [0..] $ words "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
  $ \(i, str) -> do
    liftIO $ print i
    yield str

yield :: a -> ReaderT (SourceBuilder String) IO ()
yield x = ReaderT $ \r -> do
  result <- unsafeShift $ \cont -> pure $ Left (x, Source $ unsafeReset $ cont $ pure $ Right ())
  writeIORef r $ Source $ pure result

runCoroutine :: ReaderT (SourceBuilder a) IO () -> Source IO a
runCoroutine m = Source $ unsafeReset $ do
  ref <- newIORef emptySource
  runReaderT m ref
  readIORef ref >>= unSource

printSource :: Show a => Source IO a -> IO ()
printSource (Source m) = m >>= \case
  Left (a, cont) -> print a >> printSource cont
  Right () -> pure ()
