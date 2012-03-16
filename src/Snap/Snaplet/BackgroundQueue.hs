{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.BackgroundQueue
  ( BackgroundQueue
  , backgroundQueueInit
  ) where

import           Control.Concurrent.Chan
import           Control.Monad.Trans
import           Snap.Snaplet

data BackgroundQueue a = BackgroundQueue (Chan a)

backgroundQueueInit :: SnapletInit b (BackgroundQueue a)
backgroundQueueInit = do
  makeSnaplet "backgroundQueue" "" Nothing $ do
    chan <- liftIO $ newChan
    onUnload $ putStrLn "Unloading"
    liftIO $ putStrLn "Loading"
    return (BackgroundQueue chan)
