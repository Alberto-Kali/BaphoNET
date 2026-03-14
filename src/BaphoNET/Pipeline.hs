{-# LANGUAGE OverloadedStrings #-}

module BaphoNET.Pipeline
    ( processRequest
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , ArtifactManifest(..)
    , BenchmarkSummary(..)
    , ContentBlock(..)
    , CreateJobRequest(..)
    , DocumentGroup(..)
    , ImageAsset(..)
    , ImageMention(..)
    , JobResult(..)
    , KnowledgeBundle(..)
    , NormalizedDocument(..)
    , TermDefinition(..)
    , VisionConfig(..)
    , emptyBenchmarkSummary
    )
import BaphoNET.Inference
    ( captionImageWithVision
    , cleanChunkText
    , explainTermsForTopic
    , extractKnowledgeFromTopic
    , extractMicrothemes
    , mergeMicrothemes
    )
import BaphoNET.Renderer (renderKnowledgeText)
import BaphoNET.SourceReaders (ingestSources, readUrlHttpRaw)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data PositionedSegment = PositionedSegment
    { segmentId :: T.Text
    , segmentText :: T.Text
    , segmentSourceId :: T.Text
    , segmentImageAsset :: Maybe ImageAsset
    } deriving (Eq, Show)

data ChunkBundle = ChunkBundle
    { chunkId :: T.Text
    , chunkSourceIds :: [T.Text]
    , chunkSegments :: [PositionedSegment]
    , chunkText :: T.Text
    } deriving (Eq, Show)

data TopicFragment = TopicFragment
    { fragmentTopic :: T.Text
    , fragmentChunkId :: T.Text
    , fragmentText :: T.Text
    } deriving (Eq, Show)

data TopicKnowledge = TopicKnowledge
    { topicTitle :: T.Text
    , topicText :: T.Text
    , topicTerms :: [TermDefinition]
    , topicChunkIds :: [T.Text]
    } deriving (Eq, Show)

processRequest :: AppConfig -> T.Text -> CreateJobRequest -> IO JobResult
processRequest cfg jobIdText request = do
    createDirectoryIfMissing True (storageRoot cfg </> T.unpack jobIdText </> "images")
    traceStage cfg jobIdText "ingest:start"
    documents <- ingestSources cfg (sources request)
    traceStage cfg jobIdText ("ingest:done documents=" <> T.pack (show (length documents)))
    enrichedDocuments <- mapM (enrichDocumentImages cfg jobIdText) documents
    traceStage cfg jobIdText "images:done"
    let groupedDocs = groupDocuments (userIntent request) enrichedDocuments
    traceStage cfg jobIdText ("grouping:done groups=" <> T.pack (show (length groupedDocs)))
    groups <- mapM (buildGroup cfg jobIdText) groupedDocs
    traceStage cfg jobIdText ("build-groups:done groups=" <> T.pack (show (length groups)))
    let benchmarkArtifactNames =
            [ "groups/" <> groupId grp <> "/benchmark.json"
            | grp <- groups
            , knowledgeBenchmark (groupKnowledge grp) /= Just emptyBenchmarkSummary
            ]
        imageArtifactNames =
            [ "images/" <> T.pack (takeFileNameSafe path)
            | doc <- enrichedDocuments
            , image <- normalizedImages doc
            , Just path <- [assetLocalPath image]
            ]
        manifest =
            ArtifactManifest
                { artifactNames =
                    ["job.json", "artifacts/manifest.json"]
                        <> map (\grp -> "groups/" <> groupId grp <> "/knowledge.txt") groups
                        <> map (\grp -> "groups/" <> groupId grp <> "/knowledge.md") groups
                        <> map (\grp -> "groups/" <> groupId grp <> "/metadata.json") groups
                        <> benchmarkArtifactNames
                        <> imageArtifactNames
                }
    pure JobResult
        { resultJobId = jobIdText
        , resultGroups = groups
        , resultArtifacts = manifest
        }

enrichDocumentImages :: AppConfig -> T.Text -> NormalizedDocument -> IO NormalizedDocument
enrichDocumentImages cfg jobIdText doc = do
    preparedImages <- mapM (cacheImageIfNeeded cfg jobIdText) (take (visionImageBudget (visionConfig cfg)) (normalizedImages doc))
    let remaining =
            map
                (\asset ->
                    asset
                        { assetCaptionRejectedReason = Just "image budget exceeded"
                        , assetWarnings = assetWarnings asset <> ["image budget exceeded"]
                        }
                )
                (drop (visionImageBudget (visionConfig cfg)) (normalizedImages doc))
    captionedImages <- mapM (captionImageWithVision (visionConfig cfg) (promptProfile cfg)) preparedImages
    let finalImages = captionedImages <> remaining
    pure
        doc
            { normalizedImages = finalImages
            , normalizedPlainText = rebuildPlainText finalImages (normalizedBlocks doc)
            }

cacheImageIfNeeded :: AppConfig -> T.Text -> ImageAsset -> IO ImageAsset
cacheImageIfNeeded cfg jobIdText asset
    | isRemoteImage asset = do
        let imageDir = storageRoot cfg </> T.unpack jobIdText </> "images"
            filePath = imageDir </> T.unpack (assetId asset <> ".img")
        createDirectoryIfMissing True imageDir
        downloadE <- readUrlHttpRaw (assetOriginalRef asset)
        case downloadE of
            Left err ->
                pure asset {assetWarnings = assetWarnings asset <> [err]}
            Right bytes -> do
                BL.writeFile filePath bytes
                pure asset {assetFetched = True, assetLocalPath = Just filePath}
    | otherwise = pure asset

buildGroup :: AppConfig -> T.Text -> (T.Text, [NormalizedDocument]) -> IO DocumentGroup
buildGroup cfg jobIdText (groupKey, docs) = do
    traceStage cfg jobIdText ("group:start key=" <> groupKey)
    let segments = concatMap documentSegments docs
        chunks = makeChunkBundles segments
    traceStage cfg jobIdText ("group:chunks key=" <> groupKey <> " count=" <> T.pack (show (length chunks)))
    cleanedChunks <- mapM (cleanBundle cfg) chunks
    traceStage cfg jobIdText ("group:cleaned key=" <> groupKey)
    topicFragments <- fmap concat (mapM (chunkTopics cfg) cleanedChunks)
    traceStage cfg jobIdText ("group:topic-fragments key=" <> groupKey <> " count=" <> T.pack (show (length topicFragments)))
    mergedTopics <- groupTopicFragments cfg topicFragments
    traceStage cfg jobIdText ("group:merged-topics key=" <> groupKey <> " count=" <> T.pack (show (length mergedTopics)))
    topicKnowledges <- mapM (buildTopicKnowledge cfg) mergedTopics
    traceStage cfg jobIdText ("group:knowledge key=" <> groupKey <> " count=" <> T.pack (show (length topicKnowledges)))
    let topicSections = [(topicTitle topicKnowledge, topicText topicKnowledge) | topicKnowledge <- topicKnowledges]
        allTerms = uniqueTerms (concatMap topicTerms topicKnowledges)
        finalText = renderKnowledgeText (titleForGroup groupKey docs) topicSections allTerms docs
        summaryText = T.take 280 (T.replace "\n" " " (renderSummary topicSections))
        warnings = concatMap normalizedWarnings docs <> concatMap collectImageWarnings docs
        imageMentions = collectImageMentions docs
        confidenceScore = computeConfidence cleanedChunks topicKnowledges warnings
        bundle =
            KnowledgeBundle
                { knowledgeText = finalText
                , knowledgeMarkdown = finalText
                , knowledgeAbstract = summaryText
                , knowledgeSourceIds = map normalizedSourceId docs
                , knowledgeBacklinks = map normalizedOriginalRef docs
                , knowledgeImageMentions = imageMentions
                , knowledgeTermDefinitions = allTerms
                , knowledgeConfidence = confidenceScore
                , knowledgeWarnings = warnings
                , knowledgePromptProfile = Nothing
                , knowledgeEvaluation = Nothing
                , knowledgeBenchmark = Nothing
                }
    pure
        DocumentGroup
            { groupId = slugify groupKey
            , groupTitle = titleForGroup groupKey docs
            , groupSummary = summaryText
            , groupSourceIds = map normalizedSourceId docs
            , groupKnowledge = bundle
            }

traceStage :: AppConfig -> T.Text -> T.Text -> IO ()
traceStage cfg jobIdText message =
    TIO.appendFile
        (storageRoot cfg </> T.unpack jobIdText </> "trace.log")
        (message <> "\n")

documentSegments :: NormalizedDocument -> [PositionedSegment]
documentSegments doc =
    zipWith toSegment [1 :: Int ..] (normalizedBlocks doc)
  where
    toSegment ordinal block =
        case block of
            BlockHeading txt ->
                PositionedSegment
                    { segmentId = normalizedSourceId doc <> "-seg-" <> T.pack (show ordinal)
                    , segmentText = txt
                    , segmentSourceId = normalizedSourceId doc
                    , segmentImageAsset = Nothing
                    }
            BlockParagraph txt ->
                PositionedSegment
                    { segmentId = normalizedSourceId doc <> "-seg-" <> T.pack (show ordinal)
                    , segmentText = txt
                    , segmentSourceId = normalizedSourceId doc
                    , segmentImageAsset = Nothing
                    }
            BlockImageRef imageId ->
                let mAsset = findImage imageId (normalizedImages doc)
                    imageText =
                        case mAsset >>= assetCaption of
                            Just captionText -> "Иллюстрация: " <> captionText
                            Nothing -> "Иллюстрация: изображение без надежного описания."
                in PositionedSegment
                    { segmentId = normalizedSourceId doc <> "-seg-" <> T.pack (show ordinal)
                    , segmentText = imageText
                    , segmentSourceId = normalizedSourceId doc
                    , segmentImageAsset = mAsset
                    }

makeChunkBundles :: [PositionedSegment] -> [ChunkBundle]
makeChunkBundles =
    finalize . foldl step ([], emptyChunk 1)
  where
    step (acc, currentChunk) segment =
        let newLength = chunkLength currentChunk + T.length (segmentText segment)
            shouldRoll =
                not (null (chunkSegments currentChunk))
                    && newLength > 6000
                    && (newLength > 8000 || isImageSegment segment)
        in if shouldRoll
            then (currentChunk : acc, appendSegment (emptyChunk (chunkIndex currentChunk + 1)) segment)
            else (acc, appendSegment currentChunk segment)

    finalize (acc, currentChunk)
        | null (chunkSegments currentChunk) = reverse acc
        | otherwise = reverse (currentChunk : acc)

    appendSegment chunkBundle segment =
        chunkBundle
            { chunkSegments = chunkSegments chunkBundle <> [segment]
            , chunkSourceIds = uniqueTexts (chunkSourceIds chunkBundle <> [segmentSourceId segment])
            , chunkText = T.intercalate "\n\n" (map segmentText (chunkSegments chunkBundle <> [segment]))
            }

    emptyChunk idx =
        ChunkBundle
            { chunkId = "chunk-" <> T.pack (show idx)
            , chunkSourceIds = []
            , chunkSegments = []
            , chunkText = ""
            }

    chunkLength chunkBundle = T.length (chunkText chunkBundle)
    isImageSegment segment = maybe False (const True) (segmentImageAsset segment)
    chunkIndex chunkBundle =
        case T.stripPrefix "chunk-" (chunkId chunkBundle) of
            Just numberText ->
                case reads (T.unpack numberText) of
                    (value, _) : _ -> value
                    _ -> 1
            Nothing -> 1

cleanBundle :: AppConfig -> ChunkBundle -> IO ChunkBundle
cleanBundle cfg chunkBundle = do
    cleanedText <- cleanChunkText (inferenceConfig cfg) (chunkText chunkBundle)
    pure chunkBundle {chunkText = cleanedText}

chunkTopics :: AppConfig -> ChunkBundle -> IO [TopicFragment]
chunkTopics cfg chunkBundle = do
    themes <- extractMicrothemes (inferenceConfig cfg) (chunkText chunkBundle)
    let fragments = splitIntoFragments (chunkText chunkBundle)
    pure
        [ TopicFragment
            { fragmentTopic = theme
            , fragmentChunkId = chunkId chunkBundle
            , fragmentText = fragment
            }
        | theme <- fallbackThemes themes fragments
        , Just fragment <- [bestFragmentMatch theme fragments]
        ]

groupTopicFragments :: AppConfig -> [TopicFragment] -> IO [[TopicFragment]]
groupTopicFragments cfg fragments = do
    let groupedBySeed =
            Map.elems $
                Map.fromListWith (<>)
                    [ (fragmentTopic fragment, [fragment])
                    | fragment <- fragments
                    ]
        topicLists = map (map fragmentTopic) groupedBySeed
    mergedTopicLists <- mergeMicrothemes (inferenceConfig cfg) topicLists
    pure
        [ concat
            [ matching
            | matching <- groupedBySeed
            , clusterMatches mergedThemes matching
            ]
        | mergedThemes <- mergedTopicLists
        ]

buildTopicKnowledge :: AppConfig -> [TopicFragment] -> IO TopicKnowledge
buildTopicKnowledge cfg fragments = do
    let titleText = chooseTopicTitle fragments
        orderedFragments = take 12 (uniqueTexts (map fragmentText fragments))
    knowledgeText <- extractKnowledgeFromTopic (inferenceConfig cfg) titleText orderedFragments
    termDefs <- explainTermsForTopic (inferenceConfig cfg) titleText orderedFragments
    pure
        TopicKnowledge
            { topicTitle = titleText
            , topicText = groundedTopicText knowledgeText orderedFragments
            , topicTerms = termDefs
            , topicChunkIds = uniqueTexts (map fragmentChunkId fragments)
            }

groundedTopicText :: T.Text -> [T.Text] -> T.Text
groundedTopicText generated fragments =
    let fragmentCorpus = T.toLower (T.unwords fragments)
        supportedLines =
            filter
                (\line ->
                    T.length (T.strip line) > 2
                        && (isControlLine line || any (`T.isInfixOf` fragmentCorpus) (tokenizeLine line))
                )
                (T.lines generated)
    in if length supportedLines < 3
        then T.unlines (map ("- " <>) fragments)
        else T.unlines supportedLines

tokenizeLine :: T.Text -> [T.Text]
tokenizeLine =
    filter (\token -> T.length token > 4)
        . map cleanToken
        . T.words
        . T.toLower

isControlLine :: T.Text -> Bool
isControlLine line =
    any (`T.isPrefixOf` line) ["Микротема:", "Содержание:", "-"]

splitIntoFragments :: T.Text -> [T.Text]
splitIntoFragments =
    take 24
        . filter (\line -> T.length line > 30)
        . map (trimFragment . T.strip)
        . T.splitOn "\n\n"

trimFragment :: T.Text -> T.Text
trimFragment fragment
    | T.length fragment <= 520 = fragment
    | otherwise = T.take 520 fragment <> "..."

fallbackThemes :: [T.Text] -> [T.Text] -> [T.Text]
fallbackThemes themes fragments =
    let defaults = take 4 (map (T.unwords . take 6 . T.words) fragments)
    in uniqueTexts (if null themes then defaults else themes <> defaults)

fragmentMatches :: T.Text -> T.Text -> Bool
fragmentMatches theme fragment =
    let themeTokens = tokenSet theme
        fragmentTokens = tokenSet fragment
        commonCount = length [token | token <- themeTokens, token `elem` fragmentTokens]
    in commonCount > 0 || T.toLower theme `T.isInfixOf` T.toLower fragment

bestFragmentMatch :: T.Text -> [T.Text] -> Maybe T.Text
bestFragmentMatch theme =
    fmap snd
        . List.find ((> 0) . fst)
        . List.sortBy (flip compare `on` fst)
        . map (\fragment -> (fragmentScore theme fragment, fragment))

fragmentScore :: T.Text -> T.Text -> Int
fragmentScore theme fragment =
    let themeTokens = tokenSet theme
        fragmentTokens = tokenSet fragment
        overlap = length [token | token <- themeTokens, token `elem` fragmentTokens]
        exactBoost = if T.toLower theme `T.isInfixOf` T.toLower fragment then 2 else 0
    in overlap + exactBoost

clusterMatches :: [T.Text] -> [TopicFragment] -> Bool
clusterMatches themes fragments =
    any (`elem` themes) (map fragmentTopic fragments)

chooseTopicTitle :: [TopicFragment] -> T.Text
chooseTopicTitle fragments =
    case uniqueTexts (map fragmentTopic fragments) of
        firstTheme : _ -> T.toTitle firstTheme
        [] -> "Микротема"

collectImageWarnings :: NormalizedDocument -> [T.Text]
collectImageWarnings doc =
    concatMap assetWarnings (normalizedImages doc)

collectImageMentions :: [NormalizedDocument] -> [ImageMention]
collectImageMentions docs =
    [ ImageMention
        { mentionAssetId = assetId image
        , mentionText = maybe "Иллюстрация без дополнительного описания." id (assetCaption image)
        , mentionSource = assetCaptionSource image
        }
    | doc <- docs
    , image <- normalizedImages doc
    , assetCaption image /= Nothing
    ]

computeConfidence :: [ChunkBundle] -> [TopicKnowledge] -> [T.Text] -> Double
computeConfidence chunks topics warnings =
    max 0.2 $
        0.95
            - fromIntegral (length warnings) * 0.03
            - if null chunks then 0.2 else 0.0
            - if null topics then 0.25 else 0.0

uniqueTerms :: [TermDefinition] -> [TermDefinition]
uniqueTerms =
    foldr
        (\termDef acc ->
            if any (\existing -> T.toLower (termName existing) == T.toLower (termName termDef)) acc
                then acc
                else termDef : acc
        )
        []

renderSummary :: [(T.Text, T.Text)] -> T.Text
renderSummary topicSections =
    T.intercalate " " $
        map (T.unwords . take 24 . T.words . snd) topicSections

rebuildPlainText :: [ImageAsset] -> [ContentBlock] -> T.Text
rebuildPlainText images blocks =
    T.unlines $
        concatMap
            (\block ->
                case block of
                    BlockHeading txt -> [txt]
                    BlockParagraph txt -> [txt]
                    BlockImageRef imageId ->
                        case findImage imageId images of
                            Just asset ->
                                case assetCaption asset of
                                    Just captionText -> ["Иллюстрация: " <> captionText]
                                    Nothing -> []
                            Nothing -> []
            )
            blocks

findImage :: T.Text -> [ImageAsset] -> Maybe ImageAsset
findImage imageId = List.find (\asset -> assetId asset == imageId)

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
        let keyTokens = tokenSet key
            docTokens = tokenSet (normalizedTitle doc <> " " <> normalizedPlainText doc)
            overlap = length [token | token <- keyTokens, token `elem` docTokens]
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
        $ map (\token -> (token, 1 :: Int)) (tokenSet textValue)

tokenSet :: T.Text -> [T.Text]
tokenSet =
    uniqueTexts
        . filter (\token -> T.length token > 2 && token `notElem` stopWords)
        . map cleanToken
        . T.words
        . T.toLower
  where
    stopWords =
        [ "the", "and", "for", "with", "что", "как", "или", "это", "from", "into", "для", "про", "при" ]

cleanToken :: T.Text -> T.Text
cleanToken = T.filter (\c -> c == '-' || Char.isAlphaNum c)

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

isRemoteImage :: ImageAsset -> Bool
isRemoteImage asset =
    "http://" `T.isPrefixOf` assetOriginalRef asset || "https://" `T.isPrefixOf` assetOriginalRef asset

uniqueTexts :: [T.Text] -> [T.Text]
uniqueTexts =
    foldr
        (\item acc ->
            let normalized = T.strip item
            in if T.null normalized || normalized `elem` acc then acc else normalized : acc
        )
        []

takeFileNameSafe :: FilePath -> FilePath
takeFileNameSafe = reverse . takeWhile (/= '/') . reverse
