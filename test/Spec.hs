{-# LANGUAGE OverloadedStrings #-}

module Main where

import BaphoNET.Config (loadConfig)
import BaphoNET.Domain
    ( AppConfig(..)
    , ArtifactManifest(..)
    , ContentBlock(..)
    , CreateJobRequest(..)
    , DocumentGroup(..)
    , GroupingMode(..)
    , ImageMention(..)
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
    )
import BaphoNET.Pipeline (processRequest)
import BaphoNET.SourceReaders (detectSourceType, ingestSources)
import BaphoNET.Storage (persistJobRecord)

import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removePathForcibly)
import System.FilePath ((</>))

main :: IO ()
main = do
    sourceDetectionTest
    urlDetectionTest
    groupingTest
    persistenceLayoutTest
    htmlImageNormalizationTest
    plainTextOutputTest
    putStrLn "All tests passed"

sourceDetectionTest :: IO ()
sourceDetectionTest = do
    let source =
            SourceDescriptor
                { sourceId = "s1"
                , sourceKind = SourceFile
                , sourceValue = "/tmp/example.epub"
                , sourceLabel = Nothing
                , sourceTypeHint = Nothing
                }
    assert (detectSourceType source == SourceEpub) "epub detection failed"

urlDetectionTest :: IO ()
urlDetectionTest = do
    let source =
            SourceDescriptor
                { sourceId = "u1"
                , sourceKind = SourceUrl
                , sourceValue = "https://habr.com/ru/articles/1008048/"
                , sourceLabel = Nothing
                , sourceTypeHint = Nothing
                }
    assert (detectSourceType source == SourceHtml) "url should default to html"

groupingTest :: IO ()
groupingTest = do
    let root = ".tmp-baphonet-test"
    resetDir root
    writeFile (root </> "dog-1.txt") "Dogs are loyal animals. Dogs learn commands quickly."
    writeFile (root </> "dog-2.txt") "Dog training uses reward systems. Dogs enjoy companionship."
    writeFile (root </> "cat-1.txt") "Cats are independent animals. Cats like climbing and hunting."
    cfg <- loadConfig
    let cfg' = cfg {storageRoot = root}
        request =
            CreateJobRequest
                { sources =
                    [ fileSource "dog-1" (root </> "dog-1.txt")
                    , fileSource "dog-2" (root </> "dog-2.txt")
                    , fileSource "cat-1" (root </> "cat-1.txt")
                    ]
                , userIntent = Just "dogs"
                , groupingMode = Just GroupingAuto
                , outputOptions = Just (OutputOptions False)
                }
    result <- processRequest cfg' "job-test" request
    assert (length (resultGroups result) >= 2) "expected at least 2 groups"

persistenceLayoutTest :: IO ()
persistenceLayoutTest = do
    let root = ".tmp-baphonet-persist"
    resetDir root
    cfg <- loadConfig
    now <- getCurrentTime
    let cfg' = cfg {storageRoot = root}
        groupDoc =
            DocumentGroup
                { groupId = "dogs"
                , groupTitle = "Dogs"
                , groupSummary = "Dog knowledge"
                , groupSourceIds = ["dog-1"]
                , groupKnowledge =
                    KnowledgeBundle
                        { knowledgeText = "Заголовок:\nDogs"
                        , knowledgeMarkdown = "legacy"
                        , knowledgeAbstract = "Dog knowledge"
                        , knowledgeSourceIds = ["dog-1"]
                        , knowledgeBacklinks = ["dog-source"]
                        , knowledgeImageMentions = [ImageMention "img-1" "Иллюстрация: dog chart"]
                        , knowledgeTermDefinitions = [TermDefinition "Dogs" "Домашние животные."]
                        , knowledgeConfidence = 0.9
                        , knowledgeWarnings = []
                        }
                }
        result =
            JobResult
                { resultJobId = "job-persist"
                , resultGroups = [groupDoc]
                , resultArtifacts = ArtifactManifest ["job.json", "artifacts/manifest.json"]
                }
        record =
            JobRecord
                { jobId = "job-persist"
                , jobStatus = JobSucceeded
                , jobCreatedAt = now
                , jobUpdatedAt = now
                , jobRequest =
                    CreateJobRequest
                        { sources = [fileSource "dog-1" "dog-1.txt"]
                        , userIntent = Just "dogs"
                        , groupingMode = Just GroupingAuto
                        , outputOptions = Just (OutputOptions False)
                        }
                , jobSummary = JobSummary 1 1 0
                , jobResult = Just result
                , jobError = Nothing
                }
    persistJobRecord cfg' record
    assertM (doesFileExist (root </> "job-persist" </> "job.json")) "job.json missing"
    assertM (doesFileExist (root </> "job-persist" </> "artifacts" </> "manifest.json")) "manifest.json missing"
    assertM (doesFileExist (root </> "job-persist" </> "groups" </> "dogs" </> "knowledge.txt")) "knowledge.txt missing"
    assertM (doesFileExist (root </> "job-persist" </> "groups" </> "dogs" </> "metadata.json")) "metadata.json missing"

htmlImageNormalizationTest :: IO ()
htmlImageNormalizationTest = do
    let root = ".tmp-baphonet-html"
    resetDir root
    writeFile
        (root </> "sample.html")
        "<html><head><meta property=\"og:title\" content=\"Real Title\"></head><body><p>Before image text.</p><img src=\"https://example.com/chart.png\" alt=\"Chart of throughput\"/><p>After image text.</p></body></html>"
    cfg <- loadConfig
    docs <- ingestSources cfg [fileSource "html-1" (root </> "sample.html")]
    case docs of
        doc : _ -> do
            assert (normalizedTitle doc == "Real Title") "expected cleaned title"
            assert (length (normalizedImages doc) == 1) "expected one image asset"
            assert (BlockImageRef "html-1-img-1" `elem` normalizedBlocks doc) "expected image placeholder block"
        [] -> fail "expected one normalized document"

plainTextOutputTest :: IO ()
plainTextOutputTest = do
    let root = ".tmp-baphonet-plain"
    resetDir root
    writeFile
        (root </> "sample.html")
        "<html><head><meta property=\"og:title\" content=\"Rust Performance\"></head><body><p>NextStat uses Rust and CUDA for performance.</p><img src=\"https://example.com/chart.png\" alt=\"Throughput chart\"/><p>L-BFGS-B optimizer improves model fitting.</p></body></html>"
    cfg <- loadConfig
    let cfg' = cfg {storageRoot = root}
        request =
            CreateJobRequest
                { sources = [fileSource "html-1" (root </> "sample.html")]
                , userIntent = Just "Сделай сухое знание"
                , groupingMode = Just GroupingAuto
                , outputOptions = Just (OutputOptions False)
                }
    result <- processRequest cfg' "job-plain" request
    case resultGroups result of
        grp : _ -> do
            let rendered = knowledgeText (groupKnowledge grp)
            assert ("Заголовок:" `T.isInfixOf` rendered) "missing heading section"
            assert ("Определения терминов:" `T.isInfixOf` rendered) "missing term definitions section"
            assert ("Иллюстрация:" `T.isInfixOf` rendered) "missing inline image text"
            assert (not (any (`T.isInfixOf` rendered) ["#", "*", "_", "[", "]", "(", ")"])) "unexpected markdown markers"
        [] -> fail "expected one group"

fileSource :: T.Text -> FilePath -> SourceDescriptor
fileSource sid path =
    SourceDescriptor
        { sourceId = sid
        , sourceKind = SourceFile
        , sourceValue = T.pack path
        , sourceLabel = Nothing
        , sourceTypeHint = Nothing
        }

resetDir :: FilePath -> IO ()
resetDir root = do
    exists <- doesDirectoryExist root
    if exists then removePathForcibly root else pure ()
    createDirectoryIfMissing True root

assert :: Bool -> String -> IO ()
assert condition message =
    if condition then pure () else fail message

assertM :: IO Bool -> String -> IO ()
assertM action message = action >>= (`assert` message)
