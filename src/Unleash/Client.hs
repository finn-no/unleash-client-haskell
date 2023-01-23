-- Copyright © FINN.no AS, Inc. All rights reserved.

module Unleash.Client (
    makeConfig,
    Config (..),
    registerClient,
    pollState,
    pushMetrics,
    isEnabled,
    tryIsEnabled,
) where

import Control.Concurrent.MVar
import Control.Monad (unless, void)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl, ClientEnv, ClientError, mkClientEnv)
import qualified Unleash
import Unleash.HttpClient (getAllClientFeatures, register, sendMetrics)

-- Smart constructor that initializes the mutable variables right
makeConfig :: Text -> Text -> BaseUrl -> IO Config
makeConfig applicationName instanceId serverUrl = do
    state <- newEmptyMVar
    metrics <- newMVar mempty
    now <- getCurrentTime
    metricsBucketStart <- newMVar now
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager serverUrl
    pure $
        Config
            { applicationName = applicationName,
              instanceId = instanceId,
              state = state,
              statePollIntervalInSeconds = 4,
              metrics = metrics,
              metricsBucketStart = metricsBucketStart,
              metricsPushIntervalInSeconds = 8,
              httpClientEnvironment = clientEnv
            }

data Config = Config
    { applicationName :: Text,
      instanceId :: Text,
      state :: MVar Unleash.Features,
      statePollIntervalInSeconds :: Int,
      metrics :: MVar [(Text, Bool)],
      metricsBucketStart :: MVar UTCTime,
      metricsPushIntervalInSeconds :: Int,
      httpClientEnvironment :: ClientEnv
    }

registerClient :: Config -> IO (Either ClientError ())
registerClient config = do
    now <- getCurrentTime
    let registrationPayload :: Unleash.RegisterPayload
        registrationPayload =
            Unleash.RegisterPayload
                { appName = config.applicationName,
                  instanceId = config.instanceId,
                  started = now,
                  intervalSeconds = config.metricsPushIntervalInSeconds
                }
    void <$> register config.httpClientEnvironment Nothing registrationPayload

pollState :: Config -> IO (Either ClientError ())
pollState config = do
    eitherFeatures <- getAllClientFeatures config.httpClientEnvironment Nothing
    either (const $ pure ()) (void . updateState config.state) eitherFeatures
    pure . void $ eitherFeatures
    where
        updateState state value = do
            isUpdated <- tryPutMVar state value
            unless isUpdated . void $ swapMVar state value

pushMetrics :: Config -> IO (Either ClientError ())
pushMetrics config = do
    now <- getCurrentTime
    lastBucketStart <- swapMVar config.metricsBucketStart now
    bucket <- swapMVar config.metrics mempty
    let metricsPayload =
            Unleash.MetricsPayload
                { appName = config.applicationName,
                  instanceId = config.instanceId,
                  start = lastBucketStart,
                  stop = now,
                  toggles = bucket
                }
    void <$> sendMetrics config.httpClientEnvironment Nothing metricsPayload

-- Blocks until first feature toggle set is received
isEnabled :: Config -> Text -> Unleash.Context -> IO Bool
isEnabled config feature context = do
    state <- readMVar config.state
    enabled <- Unleash.featureIsEnabled state feature context
    modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
    pure enabled

-- Returns false for all toggles until first toggle set is received
tryIsEnabled :: Config -> Text -> Unleash.Context -> IO Bool
tryIsEnabled config feature context = do
    maybeState <- tryReadMVar config.state
    case maybeState of
        Just state -> do
            enabled <- Unleash.featureIsEnabled state feature context
            modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
            pure enabled
        Nothing -> pure False
