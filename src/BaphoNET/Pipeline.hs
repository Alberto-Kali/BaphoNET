{-# LANGUAGE OverloadedStrings #-}

module BaphoNET.Pipeline
    ( processRequest
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , ArtifactManifest(..)
    , CreateJobRequest(..)
    , DocumentGroup(..)
    , JobResult(..)
    , KnowledgeBundle(..)
    , NormalizedDocument(..)
    )
import BaphoNET.Inference (refineKnowledge)
import BaphoNET.SourceReaders (ingestSources)

import qualified Data.Char as Char
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

processRequest :: AppConfig -> T.Text -> CreateJobRequest -> IO JobResult
processRequest cfg jobIdText request = do
    documents <- ingestSources cfg (sources request)
    let groupedDocs = groupDocuments (userIntent request) documents
    groups <- mapM (buildGroup cfg (userIntent request)) groupedDocs
    let manifest =
            ArtifactManifest
                { artifactNames =
                    ["job.json", "artifacts/manifest.json"]
                        <> map (\grp -> "groups/" <> groupId grp <> "/knowledge.md") groups
                        <> map (\grp -> "groups/" <> groupId grp <> "/metadata.json") groups
                }
    pure JobResult
        { resultJobId = jobIdText
        , resultGroups = groups
        , resultArtifacts = manifest
        }

buildGroup :: AppConfig -> Maybe T.Text -> (T.Text, [NormalizedDocument]) -> IO DocumentGroup
buildGroup cfg mIntent (groupKey, docs) = do
    let rawSummary = summarizeDocs docs
        rawMarkdown = renderKnowledgeMarkdown docs
    refined <- refineKnowledge (inferenceConfig cfg) mIntent docs rawMarkdown
    let finalMarkdown = maybe rawMarkdown id refined
        summaryText = T.take 280 (T.replace "\n" " " rawSummary)
        backlinks = map normalizedOriginalRef docs
        assets = List.nub (concatMap normalizedAssets docs)
        warnings = concatMap normalizedWarnings docs
        confidenceScore = if null warnings then 0.88 else 0.55
        bundle =
            KnowledgeBundle
                { knowledgeMarkdown = finalMarkdown
                , knowledgeAbstract = summaryText
                , knowledgeSourceIds = map normalizedSourceId docs
                , knowledgeBacklinks = backlinks
                , knowledgeAssetRefs = assets
                , knowledgeConfidence = confidenceScore
                , knowledgeWarnings = warnings
                }
    pure DocumentGroup
        { groupId = slugify groupKey
        , groupTitle = titleForGroup groupKey docs
        , groupSummary = summaryText
        , groupSourceIds = map normalizedSourceId docs
        , groupKnowledge = bundle
        }

groupDocuments :: Maybe T.Text -> [NormalizedDocument] -> [(T.Text, [NormalizedDocument])]
groupDocuments mIntent docs =
    Map.toList $
        foldl insertDocument Map.empty docs
  where
    insertDocument acc doc =
        let key = bestGroupKey mIntent doc (Map.keys acc)
        in Map.insertWith (<>) key [doc] acc

bestGroupKey :: Maybe T.Text -> NormalizedDocument -> [T.Text] -> T.Text
bestGroupKey mIntent doc existingKeys =
    case List.sortBy (flip compare `on` snd) (map scoreKey existingKeys) of
        (bestKey, bestScore) : _ | bestScore >= 2 -> bestKey
        _ -> seedKey mIntent doc
  where
    scoreKey key =
        let keyTokens = tokenize key
            docTokens = tokenize (normalizedTitle doc <> " " <> normalizedPlainText doc)
            overlap = Set.size (Set.intersection keyTokens docTokens)
            domainBoost =
                case normalizedDomain doc of
                    Just domain | domain `T.isInfixOf` key -> 1
                    _ -> 0
        in (key, overlap + domainBoost)

seedKey :: Maybe T.Text -> NormalizedDocument -> T.Text
seedKey mIntent doc =
    case mostRelevantTokens (normalizedTitle doc <> " " <> maybe "" id mIntent <> " " <> normalizedPlainText doc) of
        token : _ -> token
        [] -> normalizedSourceId doc

mostRelevantTokens :: T.Text -> [T.Text]
mostRelevantTokens textValue =
    take 3
        . map fst
        . List.sortBy (flip compare `on` snd)
        . Map.toList
        . Map.fromListWith (+)
        $ map (\token -> (token, 1 :: Int)) (Set.toList (tokenize textValue))

tokenize :: T.Text -> Set.Set T.Text
tokenize =
    Set.fromList
        . filter (\token -> T.length token > 2 && token `notElem` stopWords)
        . map (T.filter (`notElem` punctuation))
        . T.words
        . T.toLower
  where
    punctuation = (".,!?;:()[]{}<>\"'`/\\|+-_=*#" :: String)
    stopWords =
        [ "the", "and", "for", "with", "что", "как", "или", "это", "from", "into", "для", "про", "при" ]

summarizeDocs :: [NormalizedDocument] -> T.Text
summarizeDocs docs =
    T.unlines
        . concatMap summarizeOne
        $ docs
  where
    summarizeOne doc =
        [ "### " <> normalizedTitle doc
        , T.unwords . take 60 . T.words $ normalizedPlainText doc
        , ""
        ]

renderKnowledgeMarkdown :: [NormalizedDocument] -> T.Text
renderKnowledgeMarkdown docs =
    T.unlines $
        "# Clean knowledge"
            : ""
            : concatMap renderDoc docs
  where
    renderDoc doc =
        [ "## " <> normalizedTitle doc
        , renderBacklink doc
        , ""
        , trimNoise (normalizedMarkdown doc)
        , ""
        ]

    renderBacklink doc = "_Source: " <> normalizedOriginalRef doc <> "_"

trimNoise :: T.Text -> T.Text
trimNoise =
    T.unlines
        . filter (\line -> T.length (T.strip line) > 10 || "#" `T.isPrefixOf` T.strip line)
        . T.lines

titleForGroup :: T.Text -> [NormalizedDocument] -> T.Text
titleForGroup groupKey docs =
    case docs of
        firstDoc : _ ->
            let candidate = normalizedTitle firstDoc
            in if T.length candidate > 6 then candidate else T.toTitle groupKey
        [] -> T.toTitle groupKey

slugify :: T.Text -> T.Text
slugify =
    T.intercalate "-"
        . filter (not . T.null)
        . map (T.filter (\c -> c == '-' || Char.isAlphaNum c))
        . T.words
        . T.toLower
