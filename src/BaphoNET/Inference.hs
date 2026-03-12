{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaphoNET.Inference
    ( refineKnowledge
    ) where

import BaphoNET.Domain
    ( InferenceConfig(..)
    , NormalizedDocument(..)
    )

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, object, (.=))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Simple
    ( Request
    , getResponseBody
    , httpLBS
    , parseRequest
    , setRequestBodyJSON
    , setRequestHeader
    , setRequestMethod
    , setRequestResponseTimeout
    )

data ChatMessage = ChatMessage
    { role :: T.Text
    , content :: T.Text
    } deriving (Show, Generic, ToJSON, FromJSON)

data ChatChoice = ChatChoice
    { message :: ChatMessage
    } deriving (Show, Generic, FromJSON)

data ChatResponse = ChatResponse
    { choices :: [ChatChoice]
    } deriving (Show, Generic, FromJSON)

refineKnowledge :: InferenceConfig -> Maybe T.Text -> [NormalizedDocument] -> T.Text -> IO (Maybe T.Text)
refineKnowledge cfg mIntent docs currentSummary =
    case inferenceBaseUrl cfg of
        Nothing -> pure Nothing
        Just baseUrl -> do
            requestE <- try @SomeException (buildRequest cfg baseUrl)
            case requestE of
                Left _ -> pure Nothing
                Right request -> do
                    responseE <- try @SomeException (httpLBS (withPayload request))
                    case responseE of
                        Left _ -> pure Nothing
                        Right response ->
                            case eitherDecode (getResponseBody response) of
                                Left _ -> pure Nothing
                                Right body ->
                                    pure $
                                        case choices body of
                                            firstChoice : _ -> Just (content (message firstChoice))
                                            [] -> Nothing
  where
    withPayload request = setRequestBodyJSON payload request
    payload =
        object
            [ "model" .= inferenceModel cfg
            , "options" .= object ["num_ctx" .= inferenceContextTokens cfg]
            , "messages" .=
                [ ChatMessage "system" "You clean knowledge into compact markdown and keep only core facts."
                , ChatMessage "user" (renderPrompt cfg mIntent docs currentSummary)
                ]
            ]

buildRequest :: InferenceConfig -> String -> IO Request
buildRequest cfg baseUrl = do
    request <- parseRequest (baseUrl <> "/v1/chat/completions")
    pure $
        setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestResponseTimeout (responseTimeoutMicro (inferenceTimeoutSeconds cfg * 1000000))
            $ request

renderPrompt :: InferenceConfig -> Maybe T.Text -> [NormalizedDocument] -> T.Text -> T.Text
renderPrompt cfg mIntent docs currentSummary =
    T.unlines
        ( [ "User intent:"
          , trimToChars 500 (compactText (maybe "not provided" id mIntent))
          , ""
          , "Current deterministic draft:"
          , trimToChars summaryBudget (compactText currentSummary)
          , ""
          , "Documents:"
          ]
            <> concatMap renderDocument limitedDocs
        )
  where
    totalBudget = max 6000 (inferenceContextTokens cfg * 2)
    summaryBudget = min 5000 (totalBudget `div` 4)
    perDocBudget =
        max 1200 $
            (totalBudget - summaryBudget - 2000)
                `div` max 1 (length docs)
    limitedDocs = map (\doc -> doc {normalizedPlainText = trimToChars perDocBudget (compactText (normalizedPlainText doc))}) docs

renderDocument :: NormalizedDocument -> [T.Text]
renderDocument doc =
    [ "## " <> normalizedTitle doc
    , normalizedPlainText doc
    , ""
    ]

compactText :: T.Text -> T.Text
compactText =
    T.unwords
        . take 5000
        . T.words
        . T.replace "\r" " "
        . T.replace "\n" " "

trimToChars :: Int -> T.Text -> T.Text
trimToChars limit textValue
    | T.length textValue <= limit = textValue
    | otherwise = T.take limit textValue <> "..."
