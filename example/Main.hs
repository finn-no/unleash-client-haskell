-- Copyright © FINN.no AS, Inc. All rights reserved.

-- Example application that uses unleash-client-haskell.
--
-- Spawns a state poller thread that updates the feature toggles, a metrics
-- sender thread, and an application that continuously reads a feature toggle.
-- The application will block until the first feature toggle set is received.

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Unleash -- Should avoid or not?
import Unleash.Client

unleashServer :: BaseUrl
unleashServer = BaseUrl Http "your-unleash-server" 80 mempty

featureToggle :: Text
featureToggle = "your-feature-toggle"

main :: IO ()
main = do
    config <- makeConfig "unleash-client-haskell-example" "localhost" unleashServer
    registerApplication config
    let threads = ($ config) <$> [statePoller, metricsPusher, application]
    runConcurrently $ traverse_ Concurrently threads

application :: Config -> IO Void
application config = do
    forever do
        enabled <- isEnabled config featureToggle emptyContext
        putStrLn $ T.unpack featureToggle <> " is " <> (if enabled then "enabled" else "disabled")
        threadDelay $ 2 * 1000 * 1000

registerApplication :: Config -> IO ()
registerApplication config = do
    response <- registerClient config
    case response of
        Left error -> putStrLn $ "Could not register application (" <> show error <> ")"
        Right _ -> putStrLn "Application registered"

statePoller :: Config -> IO Void
statePoller config = do
    forever do
        response <- pollState config
        case response of
            Left newState -> putStrLn $ "Could not get state (" <> show newState <> ")"
            Right _ -> putStrLn "State received"
        threadDelay $ (statePollIntervalInSeconds config) * 1000 * 1000

metricsPusher :: Config -> IO Void
metricsPusher config = do
    forever do
        threadDelay $ (metricsPushIntervalInSeconds config) * 1000 * 1000
        response <- pushMetrics config
        case response of
            Left error -> putStrLn $ "Could not send metrics (" <> show error <> ")"
            Right _ -> putStrLn "Metrics sent"
