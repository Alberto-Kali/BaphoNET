module BaphoNET.Config
    ( loadConfig
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , InferenceConfig(..)
    , SourceType(..)
    )

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)

loadConfig :: IO AppConfig
loadConfig = do
    storage <- fromMaybe ".baphonet-storage" <$> lookupEnv "BAPHONET_STORAGE_ROOT"
    maxJobs <- maybe 32 read <$> lookupEnv "BAPHONET_MAX_JOB_SIZE"
    workers <- maybe 2 read <$> lookupEnv "BAPHONET_WORKER_CONCURRENCY"
    inferenceUrl <- lookupEnv "BAPHONET_INFERENCE_BASE_URL"
    inferenceModelName <- fmap T.pack <$> lookupEnv "BAPHONET_INFERENCE_MODEL"
    inferenceContext <- maybe 15000 read <$> lookupEnv "BAPHONET_INFERENCE_CONTEXT_TOKENS"
    inferenceTimeout <- maybe 60 read <$> lookupEnv "BAPHONET_INFERENCE_TIMEOUT_SECONDS"
    pure AppConfig
        { storageRoot = storage
        , maxJobSize = maxJobs
        , workerConcurrency = workers
        , allowedSourceTypes = [SourceHtml, SourceMarkdown, SourceText, SourceEpub]
        , inferenceConfig = InferenceConfig
            { inferenceBaseUrl = inferenceUrl
            , inferenceModel = inferenceModelName
            , inferenceContextTokens = inferenceContext
            , inferenceTimeoutSeconds = inferenceTimeout
            }
        }

_allowedSourceTypesExample :: String
_allowedSourceTypesExample = intercalate "," ["html", "md", "txt", "epub"]
