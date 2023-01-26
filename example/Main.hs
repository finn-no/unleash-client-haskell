-- Copyright © FINN.no AS, Inc. All rights reserved.

-- Example application that uses unleash-client-haskell. Spawns a state poller
-- thread that updates the feature toggles, a metrics sender thread, and an
-- application that continuously reads a feature toggle. The application will
-- block until the first feature toggle set is received.

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (asks)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Exit (die)
import Unleash (emptyContext)
import Unleash.Client (
    HasUnleash (..),
    UnleashConfig (..),
    isEnabled,
    makeUnleashConfig,
    pollToggles,
    pushMetrics,
    registerClient,
 )
import UnliftIO

unleashServer :: BaseUrl
unleashServer = BaseUrl Http "your-unleash-server" 80 mempty

secretKey :: Maybe Text
secretKey = Nothing

featureToggle :: Text
featureToggle = "your-feature-toggle"

type Program a = ReaderT AppConfig IO a

data AppConfig = AppConfig {unleashConfig :: UnleashConfig}

instance HasUnleash AppConfig where
    getUnleashConfig = unleashConfig

main :: IO ()
main = do
    config <- makeUnleashConfig "unleash-client-haskell-example" "localhost" unleashServer secretKey
    runReaderT program (AppConfig config)

program :: Program ()
program = do
    registerApplication
    let threads = [statePoller, metricsPusher, application]
    runConcurrently $ traverse_ Concurrently threads

application :: Program Void
application =
    forever do
        enabled <- isEnabled featureToggle emptyContext
        liftIO . putStrLn $ T.unpack featureToggle <> " is " <> (if enabled then "enabled" else "disabled")
        liftIO . threadDelay $ 2 * 1000 * 1000

registerApplication :: Program ()
registerApplication = do
    registerClient
        >>= liftIO . \case
            Left error -> die $ "Could not register application (" <> show error <> ")"
            Right _ -> putStrLn "Application registered"

statePoller :: Program Void
statePoller = do
    config <- asks getUnleashConfig
    forever do
        pollToggles
            >>= liftIO . \case
                Left error -> putStrLn $ "Could not get state (" <> show error <> ")"
                Right _ -> putStrLn "State received"
        liftIO . threadDelay $ config.statePollIntervalInSeconds * 1000 * 1000

metricsPusher :: Program Void
metricsPusher = do
    config <- asks getUnleashConfig
    forever do
        liftIO . threadDelay $ config.metricsPushIntervalInSeconds * 1000 * 1000
        pushMetrics
            >>= liftIO . \case
                Left error -> putStrLn $ "Could not send metrics (" <> show error <> ")"
                Right _ -> putStrLn "Metrics sent"
