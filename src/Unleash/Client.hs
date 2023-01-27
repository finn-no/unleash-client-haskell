-- Copyright © FINN.no AS, Inc. All rights reserved.

module Unleash.Client (
    makeUnleashConfig,
    UnleashConfig (..),
    HasUnleash (..),
    registerClient,
    pollToggles,
    pushMetrics,
    isEnabled,
    tryIsEnabled,
    getVariant,
    tryGetVariant,
    -- Re-exports
    Context (..),
    emptyContext,
    VariantResponse,
) where

import Control.Concurrent.MVar
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (BaseUrl, ClientEnv, ClientError, mkClientEnv)
import Unleash (Context (..), Features, MetricsPayload (..), RegisterPayload (..), VariantResponse, emptyContext, emptyVariantResponse, featureGetVariant, featureIsEnabled)
import Unleash.Internal.HttpClient (getAllClientFeatures, register, sendMetrics)

-- Smart constructor for Unleash client configuration
-- Initializes the mutable variables properly
makeUnleashConfig :: MonadIO m => Text -> Text -> BaseUrl -> Maybe Text -> m UnleashConfig
makeUnleashConfig applicationName instanceId serverUrl apiKey = do
    state <- liftIO newEmptyMVar
    metrics <- liftIO $ newMVar mempty
    metricsBucketStart <- liftIO $ getCurrentTime >>= newMVar
    manager <- newTlsManager
    let clientEnv = mkClientEnv manager serverUrl
    pure $
        UnleashConfig
            { applicationName = applicationName,
              instanceId = instanceId,
              state = state,
              statePollIntervalInSeconds = 4,
              metrics = metrics,
              metricsBucketStart = metricsBucketStart,
              metricsPushIntervalInSeconds = 8,
              apiKey = apiKey,
              httpClientEnvironment = clientEnv
            }

-- Unleash client configuration
-- Use the smart constructor or make sure the mutable metrics variables are not empty
data UnleashConfig = UnleashConfig
    { applicationName :: Text,
      instanceId :: Text,
      state :: MVar Features,
      statePollIntervalInSeconds :: Int,
      metrics :: MVar [(Text, Bool)],
      metricsBucketStart :: MVar UTCTime,
      metricsPushIntervalInSeconds :: Int,
      apiKey :: Maybe Text,
      httpClientEnvironment :: ClientEnv
    }

-- Reader monad convenience class
-- Use this to get an Unleash config from inside of an application config (for example)
class HasUnleash r where
    getUnleashConfig :: r -> UnleashConfig

-- Registers client for the Unleash server
-- Call this on application startup before calling the state poller and metrics pusher functions
registerClient :: (HasUnleash r, MonadReader r m, MonadIO m) => m (Either ClientError ())
registerClient = do
    config <- asks getUnleashConfig
    now <- liftIO getCurrentTime
    let registrationPayload :: RegisterPayload
        registrationPayload =
            RegisterPayload
                { appName = config.applicationName,
                  instanceId = config.instanceId,
                  started = now,
                  intervalSeconds = config.metricsPushIntervalInSeconds
                }
    void <$> register config.httpClientEnvironment config.apiKey registrationPayload

-- Fetches the most recent feature toggle set from the Unleash server
-- Meant to be run every statePollIntervalInSeconds
-- Non-blocking
pollToggles :: (HasUnleash r, MonadReader r m, MonadIO m) => m (Either ClientError ())
pollToggles = do
    config <- asks getUnleashConfig
    eitherFeatures <- getAllClientFeatures config.httpClientEnvironment config.apiKey
    either (const $ pure ()) (updateState config.state) eitherFeatures
    pure . void $ eitherFeatures
    where
        updateState state value = do
            isUpdated <- liftIO $ tryPutMVar state value
            liftIO . unless isUpdated . void $ swapMVar state value

-- Pushes metrics to the Unleash server
-- Meant to be run every metricsPushIntervalInSeconds
-- Blocks if the mutable metrics variables are empty
pushMetrics :: (HasUnleash r, MonadReader r m, MonadIO m) => m (Either ClientError ())
pushMetrics = do
    config <- asks getUnleashConfig
    now <- liftIO getCurrentTime
    lastBucketStart <- liftIO $ swapMVar config.metricsBucketStart now
    bucket <- liftIO $ swapMVar config.metrics mempty
    let metricsPayload =
            MetricsPayload
                { appName = config.applicationName,
                  instanceId = config.instanceId,
                  start = lastBucketStart,
                  stop = now,
                  toggles = bucket
                }
    void <$> sendMetrics config.httpClientEnvironment config.apiKey metricsPayload

-- Checks if a feature is enabled or not
-- Blocks until first feature toggle set is received
-- Blocks if the mutable metrics variables are empty
isEnabled :: (HasUnleash r, MonadReader r m, MonadIO m) => Text -> Context -> m Bool
isEnabled feature context = do
    config <- asks getUnleashConfig
    state <- liftIO . readMVar $ config.state
    enabled <- featureIsEnabled state feature context
    liftIO $ modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
    pure enabled

-- Checks if a feature is enabled or not
-- Returns false for all toggles until first toggle set is received
-- Blocks if the mutable metrics variables are empty
tryIsEnabled :: (HasUnleash r, MonadReader r m, MonadIO m) => Text -> Context -> m Bool
tryIsEnabled feature context = do
    config <- asks getUnleashConfig
    maybeState <- liftIO . tryReadMVar $ config.state
    case maybeState of
        Just state -> do
            enabled <- featureIsEnabled state feature context
            liftIO $ modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
            pure enabled
        Nothing -> pure False

-- Gets a variant
-- Blocks until first feature toggle set is received
getVariant :: (HasUnleash r, MonadReader r m, MonadIO m) => Text -> Context -> m VariantResponse
getVariant feature context = do
    config <- asks getUnleashConfig
    state <- liftIO . readMVar $ config.state
    featureGetVariant state feature context

-- Gets a variant
-- Returns an empty variant until first toggle set is received
tryGetVariant :: (HasUnleash r, MonadReader r m, MonadIO m) => Text -> Context -> m VariantResponse
tryGetVariant feature context = do
    config <- asks getUnleashConfig
    maybeState <- liftIO . tryReadMVar $ config.state
    case maybeState of
        Just state -> do
            featureGetVariant state feature context
        Nothing -> pure emptyVariantResponse
