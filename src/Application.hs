{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

{-

This module defines our application's state type and an alias for its handler
monad.

-}

module Application where

import Data.Lens.Template
import Data.Time.Clock
import Data.ByteString

import Snap.Snaplet
import Snap.Snaplet.Heist

import Snap.Snaplet.BackgroundQueue

data App = App
    { _heist :: Snaplet (Heist App)
    , _backgroundQueue :: Snaplet (BackgroundQueue ByteString)
    , _startTime :: UTCTime
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasBackgroundQueue App ByteString where
    backgroundQueueLens = subSnaplet backgroundQueue

