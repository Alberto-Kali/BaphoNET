{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module BaphoNET.Domain
    ( AppConfig(..)
    , AppEnv(..)
    , AppState(..)
    , ArtifactManifest(..)
    , ContentBlock(..)
    , CreateJobRequest(..)
    , DocumentGroup(..)
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
    , SourceDescriptor(..)
    , SourceKind(..)
    , SourceType(..)
    , TermDefinition(..)
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
    , assetWarnings :: [Text]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TermDefinition = TermDefinition
    { termName :: Text
    , termDefinition :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ImageMention = ImageMention
    { mentionAssetId :: Text
    , mentionText :: Text
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

data AppConfig = AppConfig
    { storageRoot :: FilePath
    , maxJobSize :: Int
    , workerConcurrency :: Int
    , allowedSourceTypes :: [SourceType]
    , inferenceConfig :: InferenceConfig
    } deriving (Eq, Show, Generic)

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
