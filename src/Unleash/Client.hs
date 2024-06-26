{- |
Module      : Unleash.Client
Copyright   : Copyright © FINN.no AS, Inc. All rights reserved.
License     : MIT
Stability   : experimental

Functions and types that constitute an Unleash client SDK.

This module re-exports select constructors from [unleash-client-haskell-core](https://github.com/finn-no/unleash-client-haskell-core).
-}
module Unleash.Client (
    makeUnleashConfig,
    UnleashConfig (..),
    HasUnleash (..),
    registerClient,
    registerClientWithCustomStrategies,
    pollToggles,
    pollTogglesWithCustomStrategies,
    pushMetrics,
    isEnabled,
    isEnabledWithContext,
    tryIsEnabled,
    tryIsEnabledWithContext,
    getVariant,
    tryGetVariant,
    -- Re-exports
    Context (..),
    emptyContext,
    FeatureToggleName,
    Strategy (..),
    StrategyEvaluator,
    SupportedStrategies,
    VariantResponse (..),
) where

import Control.Concurrent.MVar
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (BaseUrl, ClientEnv, ClientError, mkClientEnv)
import Unleash (
    Context (..),
    FeatureToggleName,
    Features,
    MetricsPayload (..),
    RegisterPayload (..),
    Strategy (..),
    StrategyEvaluator,
    SupportedStrategies,
    VariantResponse (..),
    defaultStrategyEvaluator,
    defaultSupportedStrategies,
    emptyContext,
    emptyVariantResponse,
    featureGetVariant,
    featureIsEnabled,
 )
import Unleash.Internal.HttpClient (getAllClientFeatures, register, sendMetrics)

-- | Smart constructor for Unleash client configuration. Initializes the mutable variables properly.
makeUnleashConfig ::
    (MonadIO m) =>
    -- | Application name.
    Text ->
    -- | Instance identifier.
    Text ->
    -- | Unleash server base URL.
    BaseUrl ->
    -- | API key for authorization.
    Maybe Text ->
    -- | Configuration instance.
    m UnleashConfig
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

-- | Unleash client configuration. Use the smart constructor or make sure the mutable metrics variables are not empty.
data UnleashConfig = UnleashConfig
    { -- | Application name.
      applicationName :: Text,
      -- | Instance identifier.
      instanceId :: Text,
      -- | Full client feature set state.
      state :: MVar Features,
      -- | Feature set state update interval.
      statePollIntervalInSeconds :: Int,
      -- | Collected metrics state.
      metrics :: MVar [(Text, Bool)],
      -- | Current metrics bucket start time.
      metricsBucketStart :: MVar UTCTime,
      -- | Metrics sending interval.
      metricsPushIntervalInSeconds :: Int,
      -- | API key for authorization.
      apiKey :: Maybe Text,
      -- | HTTP client environment.
      httpClientEnvironment :: ClientEnv
    }

-- | Reader monad convenience class. Use this to get an Unleash configuration from inside of an application configuration (for example).
class HasUnleash r where
    getUnleashConfig :: r -> UnleashConfig

-- | Register client for the Unleash server. Call this on application startup before calling the state poller and metrics pusher functions.
registerClient :: (HasUnleash r, MonadReader r m, MonadIO m) => m (Either ClientError ())
registerClient = registerClientWithCustomStrategies defaultSupportedStrategies

-- | Register client for the Unleash server. Custom strategies are added to default strategies. Call this on application startup before calling the state poller and metrics pusher functions.
registerClientWithCustomStrategies :: (HasUnleash r, MonadReader r m, MonadIO m) => SupportedStrategies -> m (Either ClientError ())
registerClientWithCustomStrategies customSupportedStrategies = do
    config <- asks getUnleashConfig
    now <- liftIO getCurrentTime
    let registrationPayload :: RegisterPayload
        registrationPayload =
            RegisterPayload
                { appName = config.applicationName,
                  instanceId = config.instanceId,
                  strategies = defaultSupportedStrategies <> customSupportedStrategies,
                  started = now,
                  intervalSeconds = config.metricsPushIntervalInSeconds
                }
    void <$> register config.httpClientEnvironment config.apiKey registrationPayload

-- | Fetch the most recent feature toggle set from the Unleash server. Meant to be run every statePollIntervalInSeconds. Non-blocking.
pollToggles :: (HasUnleash r, MonadReader r m, MonadIO m) => m (Either ClientError ())
pollToggles = pollTogglesWithCustomStrategies defaultStrategyEvaluator

-- | Fetch the most recent feature toggle set from the Unleash server. Custom strategies are added to default strategies. Meant to be run every statePollIntervalInSeconds. Non-blocking.
pollTogglesWithCustomStrategies :: (HasUnleash r, MonadReader r m, MonadIO m) => StrategyEvaluator -> m (Either ClientError ())
pollTogglesWithCustomStrategies customStrategyEvaluator = do
    config <- asks getUnleashConfig
    eitherFeatures <- getAllClientFeatures config.httpClientEnvironment strategyEvaluator config.apiKey
    either (const $ pure ()) (updateState config.state) eitherFeatures
    pure . void $ eitherFeatures
    where
        strategyEvaluator :: StrategyEvaluator
        strategyEvaluator = withCustomStrategyEvaluator customStrategyEvaluator
        updateState state value = do
            isUpdated <- liftIO $ tryPutMVar state value
            liftIO . unless isUpdated . void $ swapMVar state value

withCustomStrategyEvaluator :: StrategyEvaluator -> StrategyEvaluator
withCustomStrategyEvaluator customStrategyEvaluator featureToggleName jsonStrategy ctx = do
    defaultResult <- defaultStrategyEvaluator featureToggleName jsonStrategy ctx
    customResult <- customStrategyEvaluator featureToggleName jsonStrategy ctx
    pure $ defaultResult || customResult

-- | Push metrics to the Unleash server. Meant to be run every metricsPushIntervalInSeconds. Blocks if the mutable metrics variables are empty.
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

-- | Check if a feature is enabled or not. Blocks until first feature toggle set is received. Blocks if the mutable metrics variables are empty.
isEnabled ::
    (HasUnleash r, MonadReader r m, MonadIO m) =>
    -- | Feature toggle name.
    Text ->
    -- | Client context.
    m Bool
isEnabled feature = isEnabledWithContext feature emptyContext

-- | Check if a feature is enabled or not. Blocks until first feature toggle set is received. Blocks if the mutable metrics variables are empty.
isEnabledWithContext ::
    (HasUnleash r, MonadReader r m, MonadIO m) =>
    -- | Feature toggle name.
    Text ->
    -- | Client context.
    Context ->
    -- | Whether or not the feature toggle is enabled.
    m Bool
isEnabledWithContext feature context = do
    config <- asks getUnleashConfig
    state <- liftIO . readMVar $ config.state
    enabled <- featureIsEnabled state feature context
    liftIO $ modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
    pure enabled

-- | Check if a feature is enabled or not. Returns false for all toggles until first toggle set is received. Blocks if the mutable metrics variables are empty.
tryIsEnabled ::
    (HasUnleash r, MonadReader r m, MonadIO m) =>
    -- | Feature toggle name.
    Text ->
    -- | Client context.
    m Bool
tryIsEnabled feature = tryIsEnabledWithContext feature emptyContext

-- | Check if a feature is enabled or not. Returns false for all toggles until first toggle set is received. Blocks if the mutable metrics variables are empty.
tryIsEnabledWithContext ::
    (HasUnleash r, MonadReader r m, MonadIO m) =>
    -- | Feature toggle name.
    Text ->
    -- | Client context.
    Context ->
    -- | Whether or not the feature toggle is enabled.
    m Bool
tryIsEnabledWithContext feature context = do
    config <- asks getUnleashConfig
    maybeState <- liftIO . tryReadMVar $ config.state
    case maybeState of
        Just state -> do
            enabled <- featureIsEnabled state feature context
            liftIO $ modifyMVar_ config.metrics (\info -> pure $ (feature, enabled) : info)
            pure enabled
        Nothing -> pure False

-- | Get a variant. Blocks until first feature toggle set is received.
getVariant ::
    (HasUnleash r, MonadReader r m, MonadIO m) =>
    -- | Feature toggle name.
    Text ->
    -- | Client context.
    Context ->
    -- | Variant.
    m VariantResponse
getVariant feature context = do
    config <- asks getUnleashConfig
    state <- liftIO . readMVar $ config.state
    featureGetVariant state feature context

-- | Get a variant. Returns an empty variant until first toggle set is received.
tryGetVariant ::
    (HasUnleash r, MonadReader r m, MonadIO m) =>
    -- | Feature toggle name.
    Text ->
    -- | Client context.
    Context ->
    -- | Variant.
    m VariantResponse
tryGetVariant feature context = do
    config <- asks getUnleashConfig
    maybeState <- liftIO . tryReadMVar $ config.state
    case maybeState of
        Just state -> do
            featureGetVariant state feature context
        Nothing -> pure emptyVariantResponse
