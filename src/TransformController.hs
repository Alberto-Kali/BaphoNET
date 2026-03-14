{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TransformController
    ( ExtractionRequest(..)
    , ExtractionResponse(..)
    , extractContent
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , FetchBackend(..)
    , InferenceConfig(..)
    , NormalizedDocument(..)
    , PromptProfile(..)
    , SeleniumConfig(..)
    , SourceDescriptor(..)
    , SourceKind(..)
    , SourceType(..)
    , VisionConfig(..)
    )
import BaphoNET.SourceReaders (ingestSources)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)

data ExtractionRequest = ExtractionRequest
    { data_type :: String
    , link :: String
    , file :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ExtractionResponse = ExtractionResponse
    { extracted_text :: String
    , extracted_from :: String
    , extraction_time :: Int
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

extractContent :: ExtractionRequest -> IO ExtractionResponse
extractContent req = do
    startTime <- getCurrentTime
    let sourceValueText =
            if null (file req)
                then T.pack (link req)
                else T.pack (file req)
        sourceKindValue =
            if null (file req) then SourceUrl else SourceFile
        hintedType =
            case data_type req of
                "html" -> Just SourceHtml
                "md" -> Just SourceMarkdown
                "txt" -> Just SourceText
                "epub" -> Just SourceEpub
                _ -> Nothing
        source =
            SourceDescriptor
                { sourceId = "compat-source"
                , sourceKind = sourceKindValue
                , sourceValue = sourceValueText
                , sourceLabel = Nothing
                , sourceTypeHint = hintedType
                }
        compatConfig =
            AppConfig
                { storageRoot = ".baphonet-storage"
                , maxJobSize = 1
                , workerConcurrency = 1
                , allowedSourceTypes = [SourceHtml, SourceMarkdown, SourceText, SourceEpub, SourceUnknown]
                , fetchBackend = FetchHttp
                , seleniumConfig =
                    SeleniumConfig
                        { seleniumBrowser = "chromium"
                        , seleniumDriverUrl = "http://127.0.0.1:9515"
                        , seleniumPageTimeoutSeconds = 5
                        }
                , inferenceConfig =
                    InferenceConfig
                        { inferenceBaseUrl = Nothing
                        , inferenceModel = Nothing
                        , inferenceContextTokens = 15000
                        , inferenceTimeoutSeconds = 5
                        }
                , visionConfig =
                    VisionConfig
                        { visionBaseUrl = Nothing
                        , visionModel = Nothing
                        , visionMmprojPath = Nothing
                        , visionImageBudget = 0
                        , visionTimeoutSeconds = 5
                        }
                , promptProfile = PromptRecallGuarded
                }
    docs <- ingestSources compatConfig [source]
    endTime <- getCurrentTime
    let elapsed = round (diffUTCTime endTime startTime)
        output = T.unpack (T.intercalate "\n\n" (map normalizedPlainText docs))
    pure
        ExtractionResponse
            { extracted_text = output
            , extracted_from = T.unpack sourceValueText
            , extraction_time = elapsed
            }
