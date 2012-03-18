{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Snap.Snaplet.BackgroundQueue
  ( BackgroundQueue
  , HasBackgroundQueue
  , backgroundQueueLens
  , backgroundQueueInit
  , queueInBackground
  ) where

import           Prelude hiding (catch)

import           Control.Concurrent
import           Control.Exception (catch, SomeException)
import           Control.Monad
import           Control.Monad.Trans

import           Data.Lens.Lazy

import           Snap.Snaplet

type BackgroundChan a = Maybe (Chan (Job a))
type Action a = a -> IO ()

data BackgroundQueue a = BackgroundQueue (BackgroundChan a)

data Job a = Job a | Quit

class HasBackgroundQueue a b where
  backgroundQueueLens :: Lens (Snaplet a) (Snaplet (BackgroundQueue b))

sendToBackground :: BackgroundChan a -> Job a -> IO ()
sendToBackground Nothing _ = return ()
sendToBackground (Just c) job = writeChan c job

backgroundQueueInit :: [Action a] -> SnapletInit b (BackgroundQueue a)
backgroundQueueInit actions = do
  makeSnaplet "backgroundQueue" "" Nothing $ do
    chan <- liftIO $ foldM forkAction Nothing actions

    onUnload $ sendToBackground chan Quit

    return (BackgroundQueue chan)

forkAction :: BackgroundChan a -> Action a -> IO (BackgroundChan a)
forkAction lastChan action = do
  actionChan <- maybe newChan dupChan lastChan
  forkIO $ backgroundThread actionChan action
  return (Just actionChan)

queueInBackground :: HasBackgroundQueue a b
                  => b
                  -> Handler a a ()
queueInBackground jobData = do
  with' backgroundQueueLens $ do
    snaplet <- getSnapletState
    let BackgroundQueue chan = getL snapletValue snaplet
    liftIO $ sendToBackground chan (Job jobData)
    return ()

backgroundThread :: Chan (Job a) -> (a -> IO ()) -> IO ()
backgroundThread chan action = do
  job <- readChan chan

  case job of
    (Job a) -> do runAction (action a)
                  backgroundThread chan action
    Quit -> putStrLn "Stopping background thread"

runAction :: IO () -> IO ()
runAction action = do
  catch action
        (\e -> do let err = show (e :: SomeException)
                  putStrLn err)


