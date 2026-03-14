{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module BaphoNET.Domain
    ( AppConfig(..)
    , AppEnv(..)
    , AppState(..)
    , ArtifactManifest(..)
    , BenchmarkEntry(..)
    , BenchmarkSummary(..)
    , BrowserFetchResult(..)
    , CaptionBackend(..)
    , CaptionRejection(..)
    , CaptionSource(..)
    , ContentBlock(..)
    , CreateJobRequest(..)
    , DocumentGroup(..)
    , FetchBackend(..)
    , GroupingMode(..)
    , ImageAsset(..)
    , ImageMention(..)
    , InferenceConfig(..)
    , JobRecord(..)
    , JobResult(..)
    , JobStatus(..)
    , JobSummary(..)
    , KnowledgeBundle(..)
    , NormalizedDocument(..)
    , OutputOptions(..)
    , PromptProfile(..)
    , PromptScore(..)
    , SeleniumConfig(..)
    , SourceDescriptor(..)
    , SourceKind(..)
    , SourceType(..)
    , TermDefinition(..)
    , VisionConfig(..)
    , emptyBenchmarkSummary
    , emptyOutputOptions
    , emptySummary
    ) where

import Control.Concurrent.STM (TVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data SourceKind
    = SourceUrl
    | SourceFile
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SourceType
    = SourceHtml
    | SourceMarkdown
    | SourceText
    | SourceEpub
    | SourceUnknown
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SourceDescriptor = SourceDescriptor
    { sourceId :: Text
    , sourceKind :: SourceKind
    , sourceValue :: Text
    , sourceLabel :: Maybe Text
    , sourceTypeHint :: Maybe SourceType
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GroupingMode
    = GroupingAuto
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data OutputOptions = OutputOptions
    { includeIntermediate :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CreateJobRequest = CreateJobRequest
    { sources :: [SourceDescriptor]
    , userIntent :: Maybe Text
    , groupingMode :: Maybe GroupingMode
    , outputOptions :: Maybe OutputOptions
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data JobStatus
    = JobQueued
    | JobRunning
    | JobSucceeded
    | JobFailed
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FetchBackend
    = FetchHttp
    | FetchSelenium
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PromptProfile
    = PromptRecallMax
    | PromptRecallGuarded
    | PromptRecallStructured
    | PromptBenchmark
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)

data CaptionBackend
    = CaptionVision
    | CaptionFallback
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CaptionSource
    = CaptionFromVision
    | CaptionFromAlt
    | CaptionFromContext
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CaptionRejection = CaptionRejection
    { rejectionReason :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ContentBlock
    = BlockHeading Text
    | BlockParagraph Text
    | BlockImageRef Text
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ImageAsset = ImageAsset
    { assetId :: Text
    , assetSourceId :: Text
    , assetOriginalRef :: Text
    , assetOrdinal :: Int
    , assetSurroundingTextBefore :: Text
    , assetSurroundingTextAfter :: Text
    , assetAltText :: Maybe Text
    , assetCaption :: Maybe Text
    , assetCaptionConfidence :: Maybe Double
    , assetCaptionSource :: Maybe CaptionSource
    , assetCaptionRejectedReason :: Maybe Text
    , assetFetched :: Bool
    , assetLocalPath :: Maybe FilePath
    , assetWarnings :: [Text]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TermDefinition = TermDefinition
    { termName :: Text
    , termDefinition :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ImageMention = ImageMention
    { mentionAssetId :: Text
    , mentionText :: Text
    , mentionSource :: Maybe CaptionSource
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NormalizedDocument = NormalizedDocument
    { normalizedSourceId :: Text
    , normalizedTitle :: Text
    , normalizedSourceType :: SourceType
    , normalizedOriginalRef :: Text
    , normalizedDomain :: Maybe Text
    , normalizedPlainText :: Text
    , normalizedBlocks :: [ContentBlock]
    , normalizedImages :: [ImageAsset]
    , normalizedWarnings :: [Text]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PromptScore = PromptScore
    { unsupportedFactPenalty :: Double
    , imageHallucinationPenalty :: Double
    , coverageScore :: Double
    , termExplanationScore :: Double
    , noiseLeakagePenalty :: Double
    , sectionStructureScore :: Double
    , aggregateScore :: Double
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BenchmarkEntry = BenchmarkEntry
    { benchmarkProfile :: PromptProfile
    , benchmarkKnowledgeText :: Text
    , benchmarkScore :: PromptScore
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BenchmarkSummary = BenchmarkSummary
    { benchmarkSelectedProfile :: Maybe PromptProfile
    , benchmarkEntries :: [BenchmarkEntry]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data KnowledgeBundle = KnowledgeBundle
    { knowledgeText :: Text
    , knowledgeMarkdown :: Text
    , knowledgeAbstract :: Text
    , knowledgeSourceIds :: [Text]
    , knowledgeBacklinks :: [Text]
    , knowledgeImageMentions :: [ImageMention]
    , knowledgeTermDefinitions :: [TermDefinition]
    , knowledgeConfidence :: Double
    , knowledgeWarnings :: [Text]
    , knowledgePromptProfile :: Maybe PromptProfile
    , knowledgeEvaluation :: Maybe PromptScore
    , knowledgeBenchmark :: Maybe BenchmarkSummary
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DocumentGroup = DocumentGroup
    { groupId :: Text
    , groupTitle :: Text
    , groupSummary :: Text
    , groupSourceIds :: [Text]
    , groupKnowledge :: KnowledgeBundle
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ArtifactManifest = ArtifactManifest
    { artifactNames :: [Text]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data JobResult = JobResult
    { resultJobId :: Text
    , resultGroups :: [DocumentGroup]
    , resultArtifacts :: ArtifactManifest
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data JobSummary = JobSummary
    { summarySourceCount :: Int
    , summaryGroupCount :: Int
    , summaryWarningCount :: Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data JobRecord = JobRecord
    { jobId :: Text
    , jobStatus :: JobStatus
    , jobCreatedAt :: UTCTime
    , jobUpdatedAt :: UTCTime
    , jobRequest :: CreateJobRequest
    , jobSummary :: JobSummary
    , jobResult :: Maybe JobResult
    , jobError :: Maybe Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data InferenceConfig = InferenceConfig
    { inferenceBaseUrl :: Maybe String
    , inferenceModel :: Maybe Text
    , inferenceContextTokens :: Int
    , inferenceTimeoutSeconds :: Int
    } deriving (Eq, Show, Generic)

data VisionConfig = VisionConfig
    { visionBaseUrl :: Maybe String
    , visionModel :: Maybe Text
    , visionMmprojPath :: Maybe FilePath
    , visionImageBudget :: Int
    , visionTimeoutSeconds :: Int
    } deriving (Eq, Show, Generic)

data SeleniumConfig = SeleniumConfig
    { seleniumBrowser :: Text
    , seleniumDriverUrl :: String
    , seleniumPageTimeoutSeconds :: Int
    } deriving (Eq, Show, Generic)

data AppConfig = AppConfig
    { storageRoot :: FilePath
    , maxJobSize :: Int
    , workerConcurrency :: Int
    , allowedSourceTypes :: [SourceType]
    , fetchBackend :: FetchBackend
    , seleniumConfig :: SeleniumConfig
    , inferenceConfig :: InferenceConfig
    , visionConfig :: VisionConfig
    , promptProfile :: PromptProfile
    } deriving (Eq, Show, Generic)

data BrowserFetchResult = BrowserFetchResult
    { fetchedHtml :: Text
    , fetchedTitle :: Maybe Text
    , fetchedResolvedImages :: [(Text, Maybe Text)]
    , fetchedWarnings :: [Text]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AppState = AppState
    { jobs :: Map Text JobRecord
    , nextJobNumber :: Int
    } deriving (Eq, Show, Generic)

data AppEnv = AppEnv
    { config :: AppConfig
    , state :: TVar AppState
    }

emptyOutputOptions :: OutputOptions
emptyOutputOptions = OutputOptions {includeIntermediate = False}

emptySummary :: JobSummary
emptySummary = JobSummary
    { summarySourceCount = 0
    , summaryGroupCount = 0
    , summaryWarningCount = 0
    }

emptyBenchmarkSummary :: BenchmarkSummary
emptyBenchmarkSummary = BenchmarkSummary
    { benchmarkSelectedProfile = Nothing
    , benchmarkEntries = []
    }
