{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Snap.Snaplet.BackgroundQueue
  ( BackgroundQueue
  , HasBackgroundQueue
  , backgroundQueueLens
  , backgroundQueueInit
  , queueInBackground
  ) where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad.Trans

import           Data.Lens.Lazy

import           Snap.Snaplet

data BackgroundQueue a = BackgroundQueue (Chan (Job a))
data Job a = Job a | Quit

class HasBackgroundQueue a b where
  backgroundQueueLens :: Lens (Snaplet a) (Snaplet (BackgroundQueue b))

backgroundQueueInit :: SnapletInit b (BackgroundQueue a)
backgroundQueueInit = do
  makeSnaplet "backgroundQueue" "" Nothing $ do
    chan <- liftIO $ newChan

    liftIO $ forkIO $ backgroundThread chan

    onUnload $ do
      putStrLn "Unloading"
      writeChan chan Quit

    liftIO $ putStrLn "Loading"
    return (BackgroundQueue chan)

queueInBackground :: HasBackgroundQueue a b
                  => b
                  -> Handler a a ()
queueInBackground jobData = do
  with' backgroundQueueLens $ do
    snaplet <- getSnapletState
    let BackgroundQueue chan = getL snapletValue snaplet
    liftIO $ writeChan chan (Job jobData)
    return ()

backgroundThread :: Chan (Job a) -> IO ()
backgroundThread chan = do
  job <- readChan chan

  case job of
    (Job a) -> do putStrLn "Got Job"
                  backgroundThread chan
    Quit -> putStrLn "Stopping background thread"
