-- Copyright © FINN.no AS, Inc. All rights reserved.

module Unleash.Internal.HttpClient (
    getAllClientFeatures,
    register,
    sendMetrics,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, encode)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map, fromListWith)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Network.HTTP.Media as M
import Paths_unleash_client_haskell (version)
import Servant.API (Accept (contentTypes), Get, Header, JSON, MimeRender (mimeRender), NoContent, PostNoContent, ReqBody, type (:<|>) (..), type (:>))
import Servant.Client (ClientEnv, ClientError, client, runClientM)
import Unleash.Internal.DomainTypes (Features, fromJsonFeatures, supportedStrategies)
import Unleash.Internal.JsonTypes (FullMetricBucket (..), FullMetricsPayload (..), FullRegisterPayload (..), MetricsPayload, RegisterPayload, YesAndNoes (..))
import qualified Unleash.Internal.JsonTypes as UJT

type Register = "api" :> "client" :> "register" :> Header "Authorization" Text :> Header "Content-Type" Text :> ReqBody '[CustomJSON] FullRegisterPayload :> PostNoContent
type GetAllClientFeatures = "api" :> "client" :> "features" :> Header "Authorization" Text :> Get '[JSON] UJT.Features
type SendMetrics = "api" :> "client" :> "metrics" :> Header "Authorization" Text :> ReqBody '[CustomJSON] FullMetricsPayload :> PostNoContent
type Api = GetAllClientFeatures :<|> SendMetrics :<|> Register

getAllClientFeatures' :<|> sendMetrics' :<|> register' = client api

api :: Proxy Api
api = Proxy

type ApiKey = Text

data CustomJSON = CustomJSON

-- Remove charset=utf-8 because older versions of Unleash (e.g. 3.17.4) does not recognize it
instance Accept CustomJSON where
    contentTypes _ =
        "application" M.// "json"
            NE.:| ["application" M.// "json"]

instance {-# OVERLAPPABLE #-} ToJSON a => MimeRender CustomJSON a where
    mimeRender _ = encode

register :: MonadIO m => ClientEnv -> Maybe ApiKey -> RegisterPayload -> m (Either ClientError NoContent)
register clientEnv apiKey registerPayload = do
    let fullRegisterPayload =
            FullRegisterPayload
                { appName = registerPayload.appName,
                  instanceId = registerPayload.instanceId,
                  sdkVersion = "unleash-client-haskell:" <> (T.pack . showVersion) version,
                  strategies = supportedStrategies,
                  started = registerPayload.started,
                  interval = registerPayload.intervalSeconds * 1000
                }
    liftIO $ runClientM (register' apiKey (Just "application/json") fullRegisterPayload) clientEnv

getAllClientFeatures :: MonadIO m => ClientEnv -> Maybe ApiKey -> m (Either ClientError Features)
getAllClientFeatures clientEnv apiKey = do
    eitherFeatures <- liftIO $ runClientM (getAllClientFeatures' apiKey) clientEnv
    pure $ fromJsonFeatures <$> eitherFeatures

sendMetrics :: MonadIO m => ClientEnv -> Maybe ApiKey -> MetricsPayload -> m (Either ClientError NoContent)
sendMetrics clientEnv apiKey metricsPayload = do
    liftIO $ runClientM (sendMetrics' apiKey fullMetricsPayload) clientEnv
    where
        fullMetricsPayload :: FullMetricsPayload
        fullMetricsPayload =
            FullMetricsPayload
                { appName = metricsPayload.appName,
                  instanceId = metricsPayload.instanceId,
                  bucket =
                    FullMetricBucket
                        { start = metricsPayload.start,
                          stop = metricsPayload.stop,
                          toggles = makeMapOfYesAndNoes metricsPayload.toggles
                        }
                }
        makeMapOfYesAndNoes :: [(Text, Bool)] -> Map Text YesAndNoes
        makeMapOfYesAndNoes tuples = do
            let withSingletonLists :: [(Text, [Bool])] = (\(k, v) -> (k, [v])) <$> tuples
            let asMap :: (Map Text [Bool]) = fromListWith (++) withSingletonLists
            boolsToYesAndNoes <$> asMap
        boolsToYesAndNoes :: [Bool] -> YesAndNoes
        boolsToYesAndNoes bools = do
            let yes = length $ filter id bools
            let no = length bools - yes
            YesAndNoes yes no
