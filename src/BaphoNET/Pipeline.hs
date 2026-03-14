{-# LANGUAGE OverloadedStrings #-}

module BaphoNET.Pipeline
    ( processRequest
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , ArtifactManifest(..)
    , ContentBlock(..)
    , CreateJobRequest(..)
    , DocumentGroup(..)
    , ImageAsset(..)
    , ImageMention(..)
    , JobResult(..)
    , KnowledgeBundle(..)
    , NormalizedDocument(..)
    , TermDefinition(..)
    )
import BaphoNET.Renderer (renderKnowledgeText)
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
    enrichedDocuments <- mapM (enrichDocumentImages cfg) documents
    let groupedDocs = groupDocuments (userIntent request) enrichedDocuments
    groups <- mapM (buildGroup cfg (userIntent request)) groupedDocs
    let manifest =
            ArtifactManifest
                { artifactNames =
                    ["job.json", "artifacts/manifest.json"]
                        <> map (\grp -> "groups/" <> groupId grp <> "/knowledge.txt") groups
                        <> map (\grp -> "groups/" <> groupId grp <> "/knowledge.md") groups
                        <> map (\grp -> "groups/" <> groupId grp <> "/metadata.json") groups
                }
    pure JobResult
        { resultJobId = jobIdText
        , resultGroups = groups
        , resultArtifacts = manifest
        }

enrichDocumentImages :: AppConfig -> NormalizedDocument -> IO NormalizedDocument
enrichDocumentImages cfg doc = do
    enrichedImages <- mapM enrichImage (take 2 (normalizedImages doc))
    let remaining =
            map ensureFallbackCaption (drop 2 (normalizedImages doc))
        finalImages = enrichedImages <> remaining
        rewrittenBlocks = rewriteImageBlocks finalImages (normalizedBlocks doc)
    pure
        doc
            { normalizedImages = finalImages
            , normalizedBlocks = rewrittenBlocks
            , normalizedPlainText = blocksToPlain finalImages rewrittenBlocks
            }
  where
    enrichImage asset =
        pure asset {assetCaption = fallbackCaptionFromContext asset}

    ensureFallbackCaption asset =
        asset
            { assetCaption =
                case assetCaption asset of
                    Just captionText -> Just captionText
                    Nothing -> fallbackCaptionFromContext asset
            }

fallbackCaptionFromContext :: ImageAsset -> Maybe T.Text
fallbackCaptionFromContext asset
    | Just altText <- assetAltText asset
    , T.length (T.strip altText) > 4 =
        Just ("Изображение показывает: " <> sanitizeSentence altText)
    | not (T.null surrounding) =
        Just ("Иллюстрация связана с фрагментом: " <> surrounding <> ".")
    | otherwise = Nothing
  where
    surrounding =
        T.take 180 $
            sanitizeSentence $
                T.unwords $
                    filter (not . T.null)
                        [ assetSurroundingTextBefore asset
                        , assetSurroundingTextAfter asset
                        ]

rewriteImageBlocks images =
    map
        (\block ->
            case block of
                BlockImageRef assetIdText ->
                    case findImage assetIdText images of
                        Just asset ->
                            case assetCaption asset of
                                Just captionText -> BlockParagraph ("Иллюстрация: " <> captionText)
                                Nothing -> BlockParagraph "Иллюстрация: не добавляет новых знаний."
                        Nothing -> block
                _ -> block
        )

blocksToPlain images blocks =
    T.unwords $
        concatMap
            (\block ->
                case block of
                    BlockHeading txt -> [txt]
                    BlockParagraph txt -> [txt]
                    BlockImageRef assetIdText ->
                        case findImage assetIdText images of
                            Just asset -> maybe [] (\captionText -> [captionText]) (assetCaption asset)
                            Nothing -> []
            )
            blocks

findImage :: T.Text -> [ImageAsset] -> Maybe ImageAsset
findImage assetIdText = List.find (\asset -> assetId asset == assetIdText)

buildGroup :: AppConfig -> Maybe T.Text -> (T.Text, [NormalizedDocument]) -> IO DocumentGroup
buildGroup cfg mIntent (groupKey, docs) = do
    let termDefs = extractTermDefinitions docs
        deterministicText = renderKnowledgeText (titleForGroup groupKey docs) docs termDefs
        deterministicSummary = summarizeDocs docs
        imageMentions = collectImageMentions docs
    let refined = Nothing
    let (knowledgeTextValue, refinedTerms) =
            case joinMaybe refined of
                Just (refinedText, defs) -> (sanitizeStructuredText refinedText, if null defs then termDefs else defs)
                Nothing -> (sanitizeStructuredText deterministicText, termDefs)
        summaryText = T.take 280 (T.replace "\n" " " deterministicSummary)
        backlinks = map normalizedOriginalRef docs
        warnings = concatMap normalizedWarnings docs
        confidenceScore = if null warnings then 0.88 else 0.55
        bundle =
            KnowledgeBundle
                { knowledgeText = knowledgeTextValue
                , knowledgeMarkdown = knowledgeTextValue
                , knowledgeAbstract = summaryText
                , knowledgeSourceIds = map normalizedSourceId docs
                , knowledgeBacklinks = backlinks
                , knowledgeImageMentions = imageMentions
                , knowledgeTermDefinitions = refinedTerms
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

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just (Just value)) = Just value
joinMaybe _ = Nothing

extractTermDefinitions :: [NormalizedDocument] -> [TermDefinition]
extractTermDefinitions docs =
    take 12 $
        map makeDefinition $
            filter significantTerm $
                Set.toList $
                    Set.fromList $
                        concatMap extractTerms docs
  where
    makeDefinition termText =
        TermDefinition
            { termName = termText
            , termDefinition = explainTerm termText docs
            }

extractTerms :: NormalizedDocument -> [T.Text]
extractTerms doc =
    filter significantTerm $
        map cleanToken $
            T.words (normalizedTitle doc <> " " <> normalizedPlainText doc)

cleanToken :: T.Text -> T.Text
cleanToken =
    T.filter (\c -> Char.isAlphaNum c || c == '-' || c == '_')

significantTerm :: T.Text -> Bool
significantTerm token =
    T.length token >= 4
        && token `notElem` stopWords
        && (hasCamel token || isAbbreviation token || isTechnical token)
  where
    hasCamel txt = any Char.isUpper (T.unpack (T.drop 1 txt))
    isAbbreviation txt = T.length txt <= 10 && T.all (\c -> Char.isUpper c || Char.isDigit c) txt
    isTechnical txt = any (`T.isInfixOf` T.toLower txt) ["stat", "model", "rust", "cuda", "mcmc", "api", "glm", "cox", "next"]
    stopWords =
        [ "this", "that", "with", "from", "into", "для", "про", "как", "или", "это", "если" ]

explainTerm :: T.Text -> [NormalizedDocument] -> T.Text
explainTerm termText docs =
    let contextSnippet =
            T.unwords
                . take 20
                . concatMap (T.words . normalizedPlainText)
                $ filter (\doc -> T.toLower termText `T.isInfixOf` T.toLower (normalizedPlainText doc <> " " <> normalizedTitle doc)) docs
    in if T.null contextSnippet
        then "Важный термин из исходного материала; требует понимания в контексте темы."
        else "Термин из материала, связанный с контекстом: " <> contextSnippet <> "."

collectImageMentions :: [NormalizedDocument] -> [ImageMention]
collectImageMentions docs =
    [ ImageMention
        { mentionAssetId = assetId image
        , mentionText = maybe "Иллюстрация без дополнительного описания." id (assetCaption image)
        }
    | doc <- docs
    , image <- normalizedImages doc
    , assetCaption image /= Nothing
    ]

sanitizeStructuredText :: T.Text -> T.Text
sanitizeStructuredText =
    T.unlines
        . map sanitizeLine
        . filter (not . T.null)
        . T.lines
  where
    sanitizeLine =
        T.replace "#" ""
            . T.replace "*" ""
            . T.replace "_" ""
            . T.replace "[" ""
            . T.replace "]" ""
            . T.replace "(" ""
            . T.replace ")" ""

sanitizeSentence :: T.Text -> T.Text
sanitizeSentence = T.unwords . T.words . T.replace "\n" " "

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
        . map (T.filter (\c -> Char.isAlphaNum c || c == '-'))
        . T.words
        . T.toLower
  where
    stopWords =
        [ "the", "and", "for", "with", "что", "как", "или", "это", "from", "into", "для", "про", "при" ]

summarizeDocs :: [NormalizedDocument] -> T.Text
summarizeDocs docs =
    T.unlines
        . concatMap summarizeOne
        $ docs
  where
    summarizeOne doc =
        [ normalizedTitle doc
        , T.unwords . take 40 . T.words $ normalizedPlainText doc
        ]

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
