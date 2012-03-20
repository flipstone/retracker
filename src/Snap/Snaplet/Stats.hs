{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Stats
  ( Stats
  , HasStats
  , statsLens
  , statsInit
  , initStatValue
  , modifyStat
  ) where

import           Control.Concurrent
import           Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Lens.Lazy
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist

type StatsMap = MVar (Map.Map String Integer)
data Stats = Stats StatsMap

class HasStats a where
  statsLens :: Lens (Snaplet a) (Snaplet Stats)

statsInit :: HasHeist b => SnapletInit b Stats
statsInit = do
  makeSnaplet "stats" "" Nothing $ do
    statsMap <- liftIO $ newMVar Map.empty
    addSplices [("stats", statsSplice)]
    return (Stats statsMap)

modifyStat :: HasStats a
           => String
           -> (Maybe Integer -> Maybe Integer)
           -> Handler a a ()
modifyStat statName f = with' statsLens $ do
  Stats statsMap <- get
  liftIO $ modifyMVar_ statsMap (return . Map.alter f statName)

initStatValue :: HasStats a
              => String
              -> Integer
              -> Initializer a a ()
initStatValue statName value = with' statsLens $ do
  addPostInitHook $ \(Stats statsMap) -> do
    modifyMVar_ statsMap (return . Map.insert statName value)
    return (Stats statsMap)

statsSplice :: SnapletSplice b Stats
statsSplice = do
    (Stats statsMap) <- get
    map <- liftIO $ readMVar statsMap
    liftHeist $ mapSplices statSplices (Map.assocs map)
  where
    statSplices (statName, statValue) = runChildrenWithText [("statname", Text.pack statName),
                                                             ("statvalue", Text.pack $ show statValue)]

