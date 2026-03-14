{-# LANGUAGE OverloadedStrings #-}

module BaphoNET.Renderer
    ( renderDocumentDraft
    , renderKnowledgeText
    ) where

import BaphoNET.Domain
    ( ContentBlock(..)
    , ImageAsset(..)
    , ImageMention(..)
    , NormalizedDocument(..)
    , TermDefinition(..)
    )

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

renderDocumentDraft :: NormalizedDocument -> T.Text
renderDocumentDraft doc =
    T.intercalate "\n\n" $
        [ "Заголовок:"
        , normalizedTitle doc
        , "Материал:"
        ]
            <> map renderBlock (normalizedBlocks doc)

renderKnowledgeText :: T.Text -> [NormalizedDocument] -> [TermDefinition] -> T.Text
renderKnowledgeText groupTitle docs definitions =
    T.intercalate
        "\n\n"
        [ "Заголовок:"
        , groupTitle
        , "Суть:"
        , renderSummary docs
        , "Ключевые факты:"
        , renderKeyFacts docs
        , "Подробности:"
        , renderDetails docs
        , "Определения терминов:"
        , renderDefinitions definitions
        , "Источники:"
        , renderSources docs
        ]

renderSummary :: [NormalizedDocument] -> T.Text
renderSummary docs =
    T.intercalate " " $
        map
            (T.unwords . take 40 . T.words . normalizedPlainText)
            docs

renderKeyFacts :: [NormalizedDocument] -> T.Text
renderKeyFacts docs =
    T.unlines $
        concatMap renderDocFacts docs
  where
    renderDocFacts doc =
        map ("- " <>)
            . take 5
            . filter (not . T.null)
            . map T.strip
            . splitSentences
            $ normalizedPlainText doc

renderDetails :: [NormalizedDocument] -> T.Text
renderDetails docs =
    T.intercalate "\n\n" (concatMap renderDetailedDoc docs)
  where
    renderDetailedDoc doc =
        [ "Документ: " <> normalizedTitle doc
        , T.unlines (concatMap renderBlockWithIndent (normalizedBlocks doc))
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

renderBlockWithIndent :: ContentBlock -> [T.Text]
renderBlockWithIndent block =
    case block of
        BlockHeading txt -> ["1. " <> txt]
        BlockParagraph txt -> ["    " <> txt]
        BlockImageRef assetIdText -> ["    Иллюстрация: " <> assetIdText]

splitSentences :: T.Text -> [T.Text]
splitSentences textValue =
    filter (not . T.null)
        . map T.strip
        . T.split (`elem` (".!?" :: String))
        $ textValue
