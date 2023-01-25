-- Copyright © FINN.no AS, Inc. All rights reserved.

module Unleash.Client (
    makeConfig,
    Config (..),
    registerClient,
    pollState,
    pushMetrics,
    isEnabled,
    tryIsEnabled,
    getVariant,
    tryGetVariant
) where

import Control.Concurrent.MVar
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl, ClientEnv, ClientError, mkClientEnv)
import qualified Unleash
import Unleash.HttpClient (getAllClientFeatures, register, sendMetrics)

-- Smart constructor for Unleash client configuration
-- Initializes the mutable variables properly
makeConfig :: MonadIO m => Text -> Text -> BaseUrl -> m Config
makeConfig applicationName instanceId serverUrl = do
    state <- liftIO newEmptyMVar
    metrics <- liftIO $ newMVar mempty
    metricsBucketStart <- liftIO $ getCurrentTime >>= newMVar
    manager <- liftIO $ newManager defaultManagerSettings
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

-- Unleash client configuration
-- Use the smart constructor or make sure the mutable metrics variables are not empty
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

-- Registers client for the Unleash server
-- Call this on application startup before calling the state poller and metrics pusher functions
registerClient :: MonadIO m => Config -> m (Either ClientError ())
registerClient config = do
    now <- liftIO getCurrentTime
    let registrationPayload :: Unleash.RegisterPayload
        registrationPayload =
            Unleash.RegisterPayload
                { appName = config.applicationName,
                  instanceId = config.instanceId,
                  started = now,
                  intervalSeconds = config.metricsPushIntervalInSeconds
                }
    void <$> register config.httpClientEnvironment Nothing registrationPayload

-- Fetches the most recent feature toggle set from the Unleash server
-- Meant to be run every statePollIntervalInSeconds
-- Non-blocking
pollState :: MonadIO m => Config -> m (Either ClientError ())
pollState config = do
    eitherFeatures <- getAllClientFeatures config.httpClientEnvironment Nothing
    either (const $ pure ()) (void . updateState config.state) eitherFeatures
    pure . void $ eitherFeatures
    where
        updateState state value = do
            isUpdated <- liftIO $ tryPutMVar state value
            liftIO . unless isUpdated . void $ swapMVar state value

-- Pushes metrics to the Unleash server
-- Meant to be run every metricsPushIntervalInSeconds
-- Blocks if the mutable metrics variables are empty
pushMetrics :: MonadIO m => Config -> m (Either ClientError ())
pushMetrics config = do
    now <- liftIO getCurrentTime
    lastBucketStart <- liftIO $ swapMVar config.metricsBucketStart now
    bucket <- liftIO $ swapMVar config.metrics mempty
    let metricsPayload =
            Unleash.MetricsPayload
                { appName = config.applicationName,
                  instanceId = config.instanceId,
                  start = lastBucketStart,
                  stop = now,
                  toggles = bucket
                }
    void <$> sendMetrics config.httpClientEnvironment Nothing metricsPayload

-- Checks if a feature is enabled or not
-- Blocks until first feature toggle set is received
-- Blocks if the mutable metrics variables are empty
isEnabled :: MonadIO m => Config -> Text -> Unleash.Context -> m Bool
isEnabled config feature context = do
    state <- liftIO . readMVar $ config.state
    enabled <- Unleash.featureIsEnabled state feature context
    liftIO $ modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
    pure enabled

-- Checks if a feature is enabled or not
-- Returns false for all toggles until first toggle set is received
-- Blocks if the mutable metrics variables are empty
tryIsEnabled :: MonadIO m => Config -> Text -> Unleash.Context -> m Bool
tryIsEnabled config feature context = do
    maybeState <- liftIO . tryReadMVar $ config.state
    case maybeState of
        Just state -> do
            enabled <- Unleash.featureIsEnabled state feature context
            liftIO $ modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
            pure enabled
        Nothing -> pure False

-- Gets a variant
-- Blocks until first feature toggle set is received
getVariant :: MonadIO m => Config -> Text -> Unleash.Context -> m Unleash.VariantResponse
getVariant config feature context = do
    state <- liftIO . readMVar $ config.state
    Unleash.featureGetVariant state feature context

-- Gets a variant
-- Returns an empty variant until first toggle set is received
tryGetVariant :: MonadIO m => Config -> Text -> Unleash.Context -> m Unleash.VariantResponse
tryGetVariant config feature context = do
    maybeState <- liftIO . tryReadMVar $ config.state
    case maybeState of
        Just state -> do
            Unleash.featureGetVariant state feature context
        Nothing -> pure Unleash.emptyVariantResponse
