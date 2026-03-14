{-# LANGUAGE OverloadedStrings #-}

module BaphoNET.Renderer
    ( renderDocumentDraft
    , renderKnowledgeText
    ) where

import BaphoNET.Domain
    ( ContentBlock(..)
    , NormalizedDocument(..)
    , TermDefinition(..)
    )

import qualified Data.Text as T

renderDocumentDraft :: NormalizedDocument -> T.Text
renderDocumentDraft doc =
    T.intercalate "\n\n" $
        [ "Заголовок:"
        , normalizedTitle doc
        , "Материал:"
        ]
            <> map renderBlock (normalizedBlocks doc)

renderKnowledgeText :: T.Text -> [(T.Text, T.Text)] -> [TermDefinition] -> [NormalizedDocument] -> T.Text
renderKnowledgeText groupTitle topicSections definitions docs =
    T.intercalate
        "\n\n"
        [ "Заголовок:"
        , groupTitle
        , "Суть:"
        , renderSummary topicSections
        , "Ключевые факты:"
        , renderKeyFacts topicSections
        , "Подробности:"
        , renderTopics topicSections
        , "Определения терминов:"
        , renderDefinitions definitions
        , "Источники:"
        , renderSources docs
        ]

renderSummary :: [(T.Text, T.Text)] -> T.Text
renderSummary topicSections =
    T.intercalate " " $
        map
            (T.unwords . take 30 . T.words . snd)
            topicSections

renderKeyFacts :: [(T.Text, T.Text)] -> T.Text
renderKeyFacts topicSections =
    T.unlines $
        concatMap
            (\(topicTitle, topicText) ->
                map ("- " <>)
                    (take 3 (topicTitle : splitSentences topicText))
            )
            topicSections

renderTopics :: [(T.Text, T.Text)] -> T.Text
renderTopics topicSections =
    T.intercalate
        "\n\n"
        [ "Микротема: " <> topicTitle <> "\n" <> indentText topicText
        | (topicTitle, topicText) <- topicSections
        ]

renderDefinitions :: [TermDefinition] -> T.Text
renderDefinitions defs =
    if null defs
        then "Нет значимых терминов."
        else
            T.unlines
                [ "- " <> termName def <> ": " <> termDefinition def
                | def <- defs
                ]

renderSources :: [NormalizedDocument] -> T.Text
renderSources docs =
    T.unlines
        [ "- " <> normalizedOriginalRef doc
        | doc <- docs
        ]

renderBlock :: ContentBlock -> T.Text
renderBlock block =
    case block of
        BlockHeading txt -> txt
        BlockParagraph txt -> txt
        BlockImageRef assetIdText -> "Иллюстрация: " <> assetIdText

splitSentences :: T.Text -> [T.Text]
splitSentences textValue =
    filter (not . T.null)
        . map T.strip
        . T.split (`elem` (".!?" :: String))
        $ textValue

indentText :: T.Text -> T.Text
indentText =
    T.unlines
        . map ("    " <>)
        . filter (not . T.null)
        . map T.strip
        . T.lines
