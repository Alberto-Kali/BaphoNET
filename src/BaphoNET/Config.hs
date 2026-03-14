module BaphoNET.Config
    ( loadConfig
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , FetchBackend(..)
    , InferenceConfig(..)
    , PromptProfile(..)
    , SeleniumConfig(..)
    , SourceType(..)
    , VisionConfig(..)
    )

import Data.Char (toLower)
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
    fetchBackendValue <- fmap (map toLower) <$> lookupEnv "BAPHONET_FETCH_BACKEND"
    seleniumBrowserValue <- fmap T.pack <$> lookupEnv "BAPHONET_SELENIUM_BROWSER"
    seleniumDriver <- fromMaybe "http://127.0.0.1:9515" <$> lookupEnv "BAPHONET_SELENIUM_DRIVER_URL"
    seleniumTimeout <- maybe 20 read <$> lookupEnv "BAPHONET_SELENIUM_PAGE_TIMEOUT_SECONDS"
    visionBase <- lookupEnv "BAPHONET_VISION_BASE_URL"
    visionModelName <- fmap T.pack <$> lookupEnv "BAPHONET_VISION_MODEL"
    visionMmproj <- lookupEnv "BAPHONET_VISION_MMPROJ_PATH"
    visionBudget <- maybe 4 read <$> lookupEnv "BAPHONET_VISION_IMAGE_BUDGET"
    visionTimeout <- maybe 90 read <$> lookupEnv "BAPHONET_VISION_TIMEOUT_SECONDS"
    promptProfileValue <- fmap (map toLower) <$> lookupEnv "BAPHONET_PROMPT_PROFILE"
    pure AppConfig
        { storageRoot = storage
        , maxJobSize = maxJobs
        , workerConcurrency = workers
        , allowedSourceTypes = [SourceHtml, SourceMarkdown, SourceText, SourceEpub]
        , fetchBackend =
            case fetchBackendValue of
                Just "http" -> FetchHttp
                _ -> FetchSelenium
        , seleniumConfig =
            SeleniumConfig
                { seleniumBrowser = fromMaybe (T.pack "chromium") seleniumBrowserValue
                , seleniumDriverUrl = seleniumDriver
                , seleniumPageTimeoutSeconds = seleniumTimeout
                }
        , inferenceConfig =
            InferenceConfig
                { inferenceBaseUrl = inferenceUrl
                , inferenceModel = inferenceModelName
                , inferenceContextTokens = inferenceContext
                , inferenceTimeoutSeconds = inferenceTimeout
                }
        , visionConfig =
            VisionConfig
                { visionBaseUrl = visionBase
                , visionModel = visionModelName
                , visionMmprojPath = visionMmproj
                , visionImageBudget = visionBudget
                , visionTimeoutSeconds = visionTimeout
                }
        , promptProfile =
            case promptProfileValue of
                Just "recall-max" -> PromptRecallMax
                Just "recall-structured" -> PromptRecallStructured
                Just "benchmark" -> PromptBenchmark
                _ -> PromptRecallGuarded
        }

_allowedSourceTypesExample :: String
_allowedSourceTypesExample = intercalate "," ["html", "md", "txt", "epub"]
