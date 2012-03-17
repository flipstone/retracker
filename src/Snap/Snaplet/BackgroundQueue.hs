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

data BackgroundQueue a = BackgroundQueue (Chan (Job a))
data Job a = Job a | Quit

class HasBackgroundQueue a b where
  backgroundQueueLens :: Lens (Snaplet a) (Snaplet (BackgroundQueue b))

backgroundQueueInit :: [a -> IO ()] -> SnapletInit b (BackgroundQueue a)
backgroundQueueInit actions = do
  makeSnaplet "backgroundQueue" "" Nothing $ do
    chan <- liftIO $ newChan
    liftIO $ forkIO $ emptyMasterChan chan

    forM_ actions $ \action -> do
      liftIO $ do
        actionChan <- dupChan chan
        forkIO $ backgroundThread actionChan action

    onUnload $ writeChan chan Quit

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


emptyMasterChan :: Chan a -> IO ()
emptyMasterChan chan = readChan chan >> emptyMasterChan chan
