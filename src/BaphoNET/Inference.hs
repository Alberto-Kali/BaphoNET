{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaphoNET.Inference
    ( cleanChunkText
    , explainTermsForTopic
    , extractKnowledgeFromTopic
    , extractMicrothemes
    , mergeMicrothemes
    , captionImageWithVision
    ) where

import BaphoNET.Domain
    ( CaptionSource(..)
    , ImageAsset(..)
    , InferenceConfig(..)
    , PromptProfile(..)
    , TermDefinition(..)
    , VisionConfig(..)
    )

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, eitherDecode, object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import System.Directory (doesFileExist)

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

cleanChunkText :: InferenceConfig -> T.Text -> IO T.Text
cleanChunkText cfg chunkText = do
    let deterministic = deterministicClean chunkText
    pure deterministic

extractMicrothemes :: InferenceConfig -> T.Text -> IO [T.Text]
extractMicrothemes cfg cleanedChunk =
    if T.null (T.strip cleanedChunk)
        then pure []
        else do
            let modelThemes = []
                fallbackThemes = deterministicThemes cleanedChunk
                merged = uniqueNormalized (modelThemes <> fallbackThemes)
            pure (take 8 merged)

mergeMicrothemes :: InferenceConfig -> [[T.Text]] -> IO [[T.Text]]
mergeMicrothemes _ topicLists =
    pure (mergeBySimilarity (filter (not . null) topicLists))

extractKnowledgeFromTopic :: InferenceConfig -> T.Text -> [T.Text] -> IO T.Text
extractKnowledgeFromTopic cfg topicTitle fragments = do
    let deterministic =
            T.unlines
                [ "Микротема: " <> topicTitle
                , "Содержание:"
                , T.unlines (map ("- " <>) (take 10 fragments))
                ]
    pure deterministic

explainTermsForTopic :: InferenceConfig -> T.Text -> [T.Text] -> IO [TermDefinition]
explainTermsForTopic cfg topicTitle fragments = do
    let candidates = deterministicTermCandidates topicTitle fragments
    if null candidates
        then pure []
        else do
            pure (map (\termNameText -> TermDefinition termNameText ("Термин из микротемы: " <> topicTitle <> ".")) candidates)

captionImageWithVision :: VisionConfig -> PromptProfile -> ImageAsset -> IO ImageAsset
captionImageWithVision cfg profile asset = do
    if not (isExplicitVisionRuntime cfg)
        then pure (fallbackCaption asset "controlled vision runtime unavailable")
        else do
            payloadImage <- buildImagePayload asset
            case (visionBaseUrl cfg, payloadImage) of
                (Just baseUrl, Just imagePayload) -> do
                    requestE <- try @SomeException (buildVisionRequest cfg baseUrl)
                    case requestE of
                        Left ex -> pure (fallbackCaption asset ("vision request build failed: " <> T.pack (show ex)))
                        Right request -> do
                            responseE <- try @SomeException (httpLBS (setRequestBodyJSON (visionPayload cfg profile asset imagePayload) request))
                            case responseE of
                                Left ex -> pure (fallbackCaption asset ("vision request failed: " <> T.pack (show ex)))
                                Right response ->
                                    case eitherDecode (getResponseBody response) of
                                        Left decodeErr -> pure (fallbackCaption asset ("vision decode failed: " <> T.pack decodeErr))
                                        Right body ->
                                            case choices body of
                                                firstChoice : _ -> pure (acceptOrReject asset (T.strip (contentText (message firstChoice))))
                                                [] -> pure (fallbackCaption asset "vision returned no choices")
                _ -> pure (fallbackCaption asset "vision backend unavailable")

isExplicitVisionRuntime :: VisionConfig -> Bool
isExplicitVisionRuntime cfg =
    case visionBaseUrl cfg of
        Just baseUrl ->
            any (`T.isInfixOf` T.toLower (T.pack baseUrl)) ["llama", "openai", ":8081", ":8000"]
        Nothing -> False

callTextModel :: InferenceConfig -> T.Text -> T.Text -> T.Text -> IO T.Text
callTextModel cfg systemPrompt userPrompt fallbackText =
    case inferenceBaseUrl cfg of
        Nothing -> pure fallbackText
        Just baseUrl -> do
            requestE <- try @SomeException (buildRequest cfg baseUrl)
            case requestE of
                Left _ -> pure fallbackText
                Right request -> do
                    responseE <- try @SomeException (httpLBS (setRequestBodyJSON payload request))
                    case responseE of
                        Left _ -> pure fallbackText
                        Right response ->
                            case eitherDecode (getResponseBody response) of
                                Left _ -> pure fallbackText
                                Right body ->
                                    pure $
                                        case choices body of
                                            firstChoice : _ ->
                                                let output = T.strip (contentText (message firstChoice))
                                                in if T.null output then fallbackText else output
                                            [] -> fallbackText
  where
    payload =
        object
            [ "model" .= inferenceModel cfg
            , "options" .= object ["num_ctx" .= min 8000 (inferenceContextTokens cfg), "temperature" .= (0.1 :: Double)]
            , "messages" .=
                [ ChatMessage "system" (toJSON systemPrompt)
                , ChatMessage "user" (toJSON userPrompt)
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

buildVisionRequest :: VisionConfig -> String -> IO Request
buildVisionRequest cfg baseUrl = do
    request <- parseRequest (baseUrl <> "/v1/chat/completions")
    pure $
        setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestResponseTimeout (responseTimeoutMicro (visionTimeoutSeconds cfg * 1000000))
            $ request

visionPayload :: VisionConfig -> PromptProfile -> ImageAsset -> T.Text -> Value
visionPayload cfg profile asset imagePayload =
    object
        [ "model" .= visionModel cfg
        , "messages" .=
            [ ChatMessage "system" (toJSON (captionPrompt profile))
            , ChatMessage
                "user"
                ( toJSON
                    [ object ["type" .= ("text" :: T.Text), "text" .= renderImagePrompt asset]
                    , object
                        [ "type" .= ("image_url" :: T.Text)
                        , "image_url" .= object ["url" .= imagePayload]
                        ]
                    ]
                )
            ]
        ]

buildImagePayload :: ImageAsset -> IO (Maybe T.Text)
buildImagePayload asset =
    case assetLocalPath asset of
        Just path -> do
            exists <- doesFileExist path
            if exists
                then do
                    bytes <- BS.readFile path
                    let encoded = TE.decodeUtf8 (B64.encode bytes)
                    pure (Just ("data:image/jpeg;base64," <> encoded))
                else pure Nothing
        Nothing
            | isRemoteImage asset -> pure (Just (assetOriginalRef asset))
            | otherwise -> pure Nothing

acceptOrReject :: ImageAsset -> T.Text -> ImageAsset
acceptOrReject asset captionText
    | T.null captionText = fallbackCaption asset "empty vision caption"
    | "неинформативно" `T.isInfixOf` T.toLower captionText = fallbackCaption asset "vision marked image as uninformative"
    | weakCaption captionText = fallbackCaption asset "caption too weak for grounded use"
    | otherwise =
        asset
            { assetCaption = Just captionText
            , assetCaptionConfidence = Just 0.7
            , assetCaptionSource = Just CaptionFromVision
            , assetCaptionRejectedReason = Nothing
            }

fallbackCaption :: ImageAsset -> T.Text -> ImageAsset
fallbackCaption asset reasonText =
    case assetAltText asset of
        Just altText
            | T.length (T.strip altText) > 4 ->
                asset
                    { assetCaption = Just ("Изображение показывает: " <> T.strip altText)
                    , assetCaptionConfidence = Just 0.42
                    , assetCaptionSource = Just CaptionFromAlt
                    , assetCaptionRejectedReason = Just reasonText
                    , assetWarnings = assetWarnings asset <> [reasonText]
                    }
        _ ->
            let contextText = trimToChars 180 (compactText (assetSurroundingTextBefore asset <> " " <> assetSurroundingTextAfter asset))
            in if T.length contextText > 16
                then asset
                    { assetCaption = Just ("Иллюстрация связана с фрагментом: " <> contextText)
                    , assetCaptionConfidence = Just 0.25
                    , assetCaptionSource = Just CaptionFromContext
                    , assetCaptionRejectedReason = Just reasonText
                    , assetWarnings = assetWarnings asset <> [reasonText]
                    }
                else asset
                    { assetCaption = Nothing
                    , assetCaptionConfidence = Just 0.0
                    , assetCaptionRejectedReason = Just reasonText
                    , assetWarnings = assetWarnings asset <> [reasonText]
                    }

renderImagePrompt :: ImageAsset -> T.Text
renderImagePrompt asset =
    T.unlines
        [ "Контекст до изображения: " <> assetSurroundingTextBefore asset
        , "Контекст после изображения: " <> assetSurroundingTextAfter asset
        , "Alt текст: " <> maybe "нет" id (assetAltText asset)
        ]

cleanerPrompt :: T.Text
cleanerPrompt =
    T.unlines
        [ "Очисти фрагмент статьи."
        , "Удали usernames, даты публикации, счетчики, навигацию, рекламу, теги, кнопки шаринга."
        , "Сохрани только содержательные абзацы и подписи к изображениям."
        , "Верни только очищенный plain text."
        ]

microthemePrompt :: T.Text
microthemePrompt =
    T.unlines
        [ "Извлеки микротемы из очищенного фрагмента."
        , "Каждая строка должна содержать ровно одну короткую микротему."
        , "Не добавляй комментарии и нумерацию."
        ]

knowledgePrompt :: T.Text
knowledgePrompt =
    T.unlines
        [ "Извлеки сухое знание по одной микротеме."
        , "Используй только фрагменты из входа."
        , "Не домысливай факты."
        , "Верни plain text с коротким заголовком и списком фактов."
        ]

termPrompt :: T.Text
termPrompt =
    T.unlines
        [ "Объясни термины из микротемы."
        , "Верни строки вида Термин: Объяснение."
        , "Не включай термины, которых нет в кандидате."
        ]

captionPrompt :: PromptProfile -> T.Text
captionPrompt profile =
    case profile of
        PromptRecallMax ->
            "Опиши изображение максимально полно, но только по видимому содержимому."
        PromptRecallStructured ->
            "Опиши видимое содержимое и роль изображения без домысливания."
        _ ->
            "Опиши изображение коротко и сухо. Если оно неинформативно, ответь НЕИНФОРМАТИВНО."

deterministicClean :: T.Text -> T.Text
deterministicClean =
    T.unlines
        . filter keepLine
        . map normalizeLine
        . T.lines
  where
    keepLine line =
        let lower = T.toLower line
        in not (T.null line)
            && not (any (`T.isInfixOf` lower) ["мар в", "мин", "из песочницы", "поделиться", "комментар", "просмотр", "хабы:", "теги:"])
            && T.length line > 8
    normalizeLine = T.unwords . T.words

deterministicThemes :: T.Text -> [T.Text]
deterministicThemes textValue =
    uniqueNormalized $
        map normalizeTheme $
            take 8 $
                filter (\line -> T.length line > 12) (T.lines textValue)

deterministicTermCandidates :: T.Text -> [T.Text] -> [T.Text]
deterministicTermCandidates topicTitle fragments =
    take 10 . uniqueNormalized $
        filter significantTerm $
            map cleanToken $
                T.words (topicTitle <> " " <> T.unwords fragments)

significantTerm :: T.Text -> Bool
significantTerm token =
    T.length token >= 4
        && not (T.all (\c -> c >= '0' && c <= '9') token)
        && any (\c -> c >= 'A' && c <= 'Z') (T.unpack (T.drop 1 token) <> T.unpack token)

parseLines :: T.Text -> [T.Text]
parseLines =
    uniqueNormalized
        . map normalizeTheme
        . filter (not . T.null)
        . map T.strip
        . T.lines

parseTermDefinitions :: T.Text -> [TermDefinition]
parseTermDefinitions =
    mapMaybeTerm
        (\line ->
            let (namePart, definitionPart) = T.breakOn ":" line
            in if T.null definitionPart || T.null (T.strip namePart)
                then Nothing
                else Just
                    TermDefinition
                        { termName = T.strip namePart
                        , termDefinition = T.strip (T.drop 1 definitionPart)
                        }
        )
        . filter (not . T.null)
        . map T.strip
        . T.lines

mapMaybeTerm :: (a -> Maybe b) -> [a] -> [b]
mapMaybeTerm f =
    foldr
        (\item acc ->
            case f item of
                Just value -> value : acc
                Nothing -> acc
        )
        []

mergeBySimilarity :: [[T.Text]] -> [[T.Text]]
mergeBySimilarity =
    foldl insertCluster []
  where
    insertCluster [] themes = [themes]
    insertCluster acc themes =
        let (matched, rest) = partitionSimilar themes acc
        in case matched of
            [] -> acc <> [themes]
            firstMatch : _ -> normalizeCluster [themes, firstMatch] : filter (/= firstMatch) rest

    partitionSimilar themes =
        foldr
            (\candidate (matched, rest) ->
                if similarThemes themes candidate
                    then (candidate : matched, rest)
                    else (matched, candidate : rest)
            )
            ([], [])

    normalizeCluster = uniqueNormalized . concatMap (map normalizeTheme)

similarThemes :: [T.Text] -> [T.Text] -> Bool
similarThemes left right =
    any
        (\l -> any (\r -> themeSimilarity l r >= 0.45) right)
        left

themeSimilarity :: T.Text -> T.Text -> Double
themeSimilarity left right =
    let leftTokens = tokenSet left
        rightTokens = tokenSet right
        commonCount = length [token | token <- leftTokens, token `elem` rightTokens]
        denom = max 1 (max (length leftTokens) (length rightTokens))
    in fromIntegral commonCount / fromIntegral denom

tokenSet :: T.Text -> [T.Text]
tokenSet =
    uniqueNormalized
        . map cleanToken
        . filter (\token -> T.length token > 3)
        . T.words
        . T.toLower

cleanToken :: T.Text -> T.Text
cleanToken = T.filter (\c -> T.any (== c) allowed || (c >= 'a' && c <= 'z') || (c >= 'а' && c <= 'я') || (c >= '0' && c <= '9'))
  where
    allowed = "-_"

normalizeTheme :: T.Text -> T.Text
normalizeTheme = T.toLower . T.unwords . T.words . cleanPunctuation

cleanPunctuation :: T.Text -> T.Text
cleanPunctuation = T.map (\c -> if c `elem` (",.;:!?()[]{}\"" :: String) then ' ' else c)

uniqueNormalized :: [T.Text] -> [T.Text]
uniqueNormalized =
    foldr
        (\item acc ->
            let normalized = T.strip item
            in if T.null normalized || normalized `elem` acc then acc else normalized : acc
        )
        []

weakCaption :: T.Text -> Bool
weakCaption captionText =
    let lowered = T.toLower captionText
    in T.length captionText < 12
        || any (`T.isInfixOf` lowered) ["maybe", "possibly", "could be", "seems like"]

isRemoteImage :: ImageAsset -> Bool
isRemoteImage asset =
    "http://" `T.isPrefixOf` assetOriginalRef asset || "https://" `T.isPrefixOf` assetOriginalRef asset

compactText :: T.Text -> T.Text
compactText =
    T.unwords . T.words . T.replace "\r" " " . T.replace "\n" " "

trimToChars :: Int -> T.Text -> T.Text
trimToChars limit textValue
    | T.length textValue <= limit = textValue
    | otherwise = T.take limit textValue <> "..."
