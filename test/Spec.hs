{-# LANGUAGE OverloadedStrings #-}

module Main where

import BaphoNET.Config (loadConfig)
import BaphoNET.Domain
    ( AppConfig(..)
    , ArtifactManifest(..)
    , CreateJobRequest(..)
    , DocumentGroup(..)
    , GroupingMode(..)
    , JobRecord(..)
    , JobResult(..)
    , JobStatus(..)
    , JobSummary(..)
    , KnowledgeBundle(..)
    , OutputOptions(..)
    , SourceDescriptor(..)
    , SourceKind(..)
    , SourceType(..)
    )
import BaphoNET.Pipeline (processRequest)
import BaphoNET.SourceReaders (detectSourceType)
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
    htmlSanitizationTest
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
    assert (show (detectSourceType source) == "SourceEpub") "epub detection failed"

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
    exists <- doesDirectoryExist root
    if exists then removePathForcibly root else pure ()
    createDirectoryIfMissing True root
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
    exists <- doesDirectoryExist root
    if exists then removePathForcibly root else pure ()
    createDirectoryIfMissing True root
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
                        { knowledgeMarkdown = "# Dogs"
                        , knowledgeAbstract = "Dog knowledge"
                        , knowledgeSourceIds = ["dog-1"]
                        , knowledgeBacklinks = ["dog-source"]
                        , knowledgeAssetRefs = []
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
                , jobSummary =
                    JobSummary
                        { summarySourceCount = 1
                        , summaryGroupCount = 1
                        , summaryWarningCount = 0
                        }
                , jobResult = Just result
                , jobError = Nothing
                }
    persistJobRecord cfg' record
    assertM (doesFileExist (root </> "job-persist" </> "job.json")) "job.json missing"
    assertM (doesFileExist (root </> "job-persist" </> "artifacts" </> "manifest.json")) "manifest.json missing"
    assertM (doesFileExist (root </> "job-persist" </> "groups" </> "dogs" </> "knowledge.md")) "knowledge.md missing"
    assertM (doesFileExist (root </> "job-persist" </> "groups" </> "dogs" </> "metadata.json")) "metadata.json missing"

htmlSanitizationTest :: IO ()
htmlSanitizationTest = do
    let root = ".tmp-baphonet-html"
    exists <- doesDirectoryExist root
    if exists then removePathForcibly root else pure ()
    createDirectoryIfMissing True root
    writeFile
        (root </> "sample.html")
        "<html><head><title>![ ](data:image/svg+xml;base64,AAAA) Real Title</title></head><body><nav>navigation</nav><h1>Real Title</h1><p>Useful body text.</p></body></html>"
    cfg <- loadConfig
    let cfg' = cfg {storageRoot = root}
        request =
            CreateJobRequest
                { sources = [fileSource "html-1" (root </> "sample.html")]
                , userIntent = Nothing
                , groupingMode = Just GroupingAuto
                , outputOptions = Just (OutputOptions False)
                }
    result <- processRequest cfg' "job-html" request
    let firstGroup = head (resultGroups result)
        rendered = knowledgeMarkdown (groupKnowledge firstGroup)
    assert ("Real Title" `T.isInfixOf` groupTitle firstGroup) "expected cleaned title"
    assert (not ("data:image" `T.isInfixOf` rendered)) "data uri should be stripped"
    assert ("Useful body text." `T.isInfixOf` rendered) "expected useful body text"

fileSource :: T.Text -> FilePath -> SourceDescriptor
fileSource sid path =
    SourceDescriptor
        { sourceId = sid
        , sourceKind = SourceFile
        , sourceValue = T.pack path
        , sourceLabel = Nothing
        , sourceTypeHint = Nothing
        }

assert :: Bool -> String -> IO ()
assert condition message =
    if condition then pure () else fail message

assertM :: IO Bool -> String -> IO ()
assertM action message = action >>= (`assert` message)
