
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}
module Coroutine where

import GHC.Prim
import GHC.IO (IO(..))
import Data.Foldable
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

unsafeReset :: IO a -> IO a
unsafeReset (IO a) = IO (reset# a)

unsafeShift :: ((IO a -> IO b) -> IO b) -> IO a
unsafeShift f = IO $ shift# $ \k -> case f (\(IO a) -> IO (k a)) of IO b -> b

data Step a r = More !a !r | Done

newtype Source m a = Source { unSource :: m (Step a (Source m a)) }

emptySource :: Applicative m => Source m a
emptySource = Source $ pure Done

type SourceBuilder a = IORef (Source IO a)

loremIpsum :: ReaderT (SourceBuilder String) IO ()
loremIpsum = for_ (zip [0 :: Int ..] $ words "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
  $ \(i, str) -> do
    liftIO $ print i
    yield str

yield :: a -> ReaderT (SourceBuilder a) IO ()
yield x = ReaderT $ \r -> do
  result <- unsafeShift $ \cont -> pure $ More x $ Source $ unsafeReset $ cont $ pure Done
  writeIORef r $ Source $ pure result
{-# INLINE yield #-}

runCoroutine :: ReaderT (SourceBuilder a) IO () -> Source IO a
runCoroutine m = Source $ unsafeReset $ do
  ref <- newIORef emptySource
  runReaderT m ref
  readIORef ref >>= unSource

printSource :: Show a => Source IO a -> IO ()
printSource (Source m) = m >>= \case
  More a cont -> print a >> printSource cont
  Done -> pure ()

sumSource :: Source IO Int -> IO Int
sumSource = go 0 where
  go !acc (Source m) = m >>= \case
    More a cont -> go (acc + a) cont
    Done -> pure acc
{-# INLINE sumSource #-}

drain :: Source IO a -> IO ()
drain (Source m) = m >>= \case
  More _ cont -> drain cont
  Done -> pure ()
