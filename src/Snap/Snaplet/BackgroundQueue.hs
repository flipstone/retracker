{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Snap.Snaplet.BackgroundQueue
  ( BackgroundQueue
  , HasBackgroundQueue
  , backgroundQueueLens
  , backgroundQueueInit
  , queueInBackground
  ) where

import           Prelude hiding (catch, (.))

import qualified Control.Category as Category
import           Control.Concurrent
import           Control.Exception (catch, SomeException)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans

import           Data.Lens.Lazy

import           Snap.Snaplet hiding (Handler)
import qualified Snap.Snaplet as Snaplet

data Job a b = Job a (Lens (Snaplet b) (Snaplet b)) (Snaplet b)
             | Quit
type BackgroundChan a b = Maybe (Chan (Job a b))
type Action b a = a -> Handler b b ()

data LensedIO b v a = LensedIO {
   lensedAction :: Lens b v
                -> b
                -> v
                -> IO (b, v, a)
  }

newtype Handler b v a =
   Handler (LensedIO (Snaplet b) (Snaplet v) a)
 deriving (Monad, MonadState (Snaplet v))

instance Monad (LensedIO b v) where
  return a = LensedIO $ \_ b v -> return (b, v, a)
  (LensedIO h) >>= f = LensedIO $ \l b v -> do
                                    (b', v', a) <- h l b v
                                    let LensedIO h' = f a
                                    h' l b' v'

instance MonadState v (LensedIO b v) where
  get = LensedIO $ \_ b v -> return (b, v, v)
  put v' = LensedIO $ \_ b _ -> return (b, v', ())

instance MonadSnaplet Handler where
  withTop' lens (Handler lensedIO) = Handler $ LensedIO $ \l b v -> do
    let v' = getL lens b
        LensedIO action = lensedIO
    (b', _, a) <- action lens b v'
    return (b', v, a)

  with' l h = do
      l' <- getLens
      withTop' (l `composeLens` l') h
    where composeLens = (Category..)

  getLens = Handler $ LensedIO $ \l b v -> return (b, v, l)
  getOpaqueConfig = undefined

data BackgroundQueue b a = BackgroundQueue (BackgroundChan b a)

class HasBackgroundQueue a b where
  backgroundQueueLens :: Lens (Snaplet a) (Snaplet (BackgroundQueue b a))

writeBGChan :: BackgroundChan a b -> Job a b -> IO ()
writeBGChan Nothing _ = return ()
writeBGChan (Just c) job = writeChan c job

backgroundQueueInit :: [Action b a] -> SnapletInit b (BackgroundQueue a b)
backgroundQueueInit actions = do
  makeSnaplet "backgroundQueue" "" Nothing $ do
    chan <- liftIO $ foldM forkAction Nothing actions

    onUnload $ writeBGChan chan Quit

    return (BackgroundQueue chan)

forkAction :: BackgroundChan a b -> Action b a -> IO (BackgroundChan a b)
forkAction lastChan action = do
  actionChan <- maybe newChan dupChan lastChan
  forkIO $ backgroundThread actionChan $ \jobData lens snaplet -> do
    let Handler lensed = action jobData
    lensedAction lensed lens snaplet snaplet
    return ()

  return (Just actionChan)

queueInBackground :: HasBackgroundQueue a b
                  => b
                  -> Snaplet.Handler a a ()
queueInBackground jobData = do
  appLens <- getLens
  appState <- get
  with' backgroundQueueLens $ do
    BackgroundQueue chan <- get
    liftIO $ writeBGChan chan (Job jobData appLens appState)
    return ()

backgroundThread :: Chan (Job a b)
                 -> (a -> Lens (Snaplet b) (Snaplet b) -> (Snaplet b) -> IO ())
                 -> IO ()
backgroundThread chan action = do
  job <- readChan chan

  case job of
    (Job a lens snaplet) -> do runAction (action a lens snaplet)
                               backgroundThread chan action
    Quit -> putStrLn "Stopping background thread"

runAction :: IO () -> IO ()
runAction action = do
  catch action
        (\e -> do let err = show (e :: SomeException)
                  putStrLn err)


