{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaphoNET.Inference
    ( captionImageAsset
    , refineKnowledge
    ) where

import BaphoNET.Domain
    ( ImageAsset(..)
    , InferenceConfig(..)
    , NormalizedDocument(..)
    , TermDefinition(..)
    )

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, eitherDecode, object, withObject, (.:), (.=))
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
    , content :: Value
    } deriving (Show, Generic, ToJSON)

data ChatChoice = ChatChoice
    { message :: AssistantMessage
    } deriving (Show, Generic, FromJSON)

data AssistantMessage = AssistantMessage
    { contentText :: T.Text
    } deriving (Show, Generic)

instance FromJSON AssistantMessage where
    parseJSON =
        withObject "AssistantMessage" $ \obj ->
            AssistantMessage <$> obj .: "content"

data ChatResponse = ChatResponse
    { choices :: [ChatChoice]
    } deriving (Show, Generic, FromJSON)

refineKnowledge :: InferenceConfig -> Maybe T.Text -> [NormalizedDocument] -> T.Text -> IO (Maybe (T.Text, [TermDefinition]))
refineKnowledge cfg mIntent docs currentDraft =
    case inferenceBaseUrl cfg of
        Nothing -> pure Nothing
        Just baseUrl -> do
            requestE <- try @SomeException (buildRequest cfg baseUrl)
            case requestE of
                Left _ -> pure Nothing
                Right request -> do
                    responseE <- try @SomeException (httpLBS (setRequestBodyJSON payload request))
                    case responseE of
                        Left _ -> pure Nothing
                        Right response ->
                            case eitherDecode (getResponseBody response) of
                                Left _ -> pure Nothing
                                Right body ->
                                    pure $
                                        case choices body of
                                            firstChoice : _ ->
                                                Just (contentText (message firstChoice), [])
                                            [] -> Nothing
  where
    payload =
        object
            [ "model" .= inferenceModel cfg
            , "options" .= object ["num_ctx" .= inferenceContextTokens cfg]
            , "messages" .=
                [ ChatMessage "system" (toTextValue synthesisPrompt)
                , ChatMessage "user" (toTextValue (renderPrompt mIntent docs currentDraft))
                ]
            ]

captionImageAsset :: InferenceConfig -> ImageAsset -> IO (Maybe T.Text)
captionImageAsset cfg asset =
    case (inferenceBaseUrl cfg, isRemoteImage asset) of
        (Just baseUrl, True) -> do
            requestE <- try @SomeException (buildRequest cfg baseUrl)
            case requestE of
                Left _ -> pure Nothing
                Right request -> do
                    responseE <- try @SomeException (httpLBS (setRequestBodyJSON payload request))
                    case responseE of
                        Left _ -> pure Nothing
                        Right response ->
                            case eitherDecode (getResponseBody response) of
                                Left _ -> pure Nothing
                                Right body ->
                                    pure $
                                        case choices body of
                                            firstChoice : _ -> Just (T.strip (contentText (message firstChoice)))
                                            [] -> Nothing
          where
            payload =
                object
                    [ "model" .= inferenceModel cfg
                    , "options" .= object ["num_ctx" .= inferenceContextTokens cfg]
                    , "messages" .=
                        [ ChatMessage "system" (toTextValue imagePrompt)
                        , ChatMessage
                            "user"
                            ( toJSON
                                [ object ["type" .= ("text" :: T.Text), "text" .= renderImagePrompt asset]
                                , object
                                    [ "type" .= ("image_url" :: T.Text)
                                    , "image_url" .= object ["url" .= assetOriginalRef asset]
                                    ]
                                ]
                            )
                        ]
                    ]
        _ -> pure (fallbackCaption asset)

buildRequest :: InferenceConfig -> String -> IO Request
buildRequest cfg baseUrl = do
    request <- parseRequest (baseUrl <> "/v1/chat/completions")
    pure $
        setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestResponseTimeout (responseTimeoutMicro (inferenceTimeoutSeconds cfg * 1000000))
            $ request

renderPrompt :: Maybe T.Text -> [NormalizedDocument] -> T.Text -> T.Text
renderPrompt mIntent docs currentDraft =
    T.unlines
        [ "Интенция пользователя:"
        , maybe "не указана" id mIntent
        , ""
        , "Черновик знания:"
        , trimToChars 12000 (compactText currentDraft)
        , ""
        , "Контекст документов:"
        , trimToChars 6000 (compactText (T.unlines (map normalizedPlainText docs)))
        ]

synthesisPrompt :: T.Text
synthesisPrompt =
    T.unlines
        [ "Сформируй structured plain text без markdown."
        , "Стиль сухой и factual."
        , "Объясняй важные термины коротко при первом упоминании."
        , "В конце добавь блок 'Определения терминов'."
        , "Используй только обычный текст, двоеточия, нумерацию, списки с дефисом и отступы."
        ]

imagePrompt :: T.Text
imagePrompt =
    T.unlines
        [ "Опиши изображение коротко и сухо."
        , "Укажи, что изображено и зачем это важно для понимания статьи."
        , "Если изображение неинформативно, ответь: НЕИНФОРМАТИВНО."
        ]

renderImagePrompt :: ImageAsset -> T.Text
renderImagePrompt asset =
    T.unlines
        [ "Контекст до изображения: " <> assetSurroundingTextBefore asset
        , "Контекст после изображения: " <> assetSurroundingTextAfter asset
        , "Alt текст: " <> maybe "нет" id (assetAltText asset)
        ]

toTextValue :: T.Text -> Value
toTextValue textValue = object ["type" .= ("text" :: T.Text), "text" .= textValue]

isRemoteImage :: ImageAsset -> Bool
isRemoteImage asset =
    "http://" `T.isPrefixOf` assetOriginalRef asset || "https://" `T.isPrefixOf` assetOriginalRef asset

fallbackCaption :: ImageAsset -> Maybe T.Text
fallbackCaption asset =
    case assetAltText asset of
        Just altText | T.length (T.strip altText) > 4 -> Just ("Изображение показывает: " <> T.strip altText)
        _ -> Nothing

compactText :: T.Text -> T.Text
compactText =
    T.unwords . T.words . T.replace "\r" " " . T.replace "\n" " "

trimToChars :: Int -> T.Text -> T.Text
trimToChars limit textValue
    | T.length textValue <= limit = textValue
    | otherwise = T.take limit textValue <> "..."
