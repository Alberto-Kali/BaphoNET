{-# LANGUAGE OverloadedStrings #-}

module BaphoNET.Storage
    ( ensureStorageRoot
    , persistJobRecord
    , readArtifact
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , DocumentGroup(..)
    , JobRecord(..)
    , JobResult(..)
    , KnowledgeBundle(..)
    )

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

ensureStorageRoot :: AppConfig -> IO ()
ensureStorageRoot cfg = createDirectoryIfMissing True (storageRoot cfg)

persistJobRecord :: AppConfig -> JobRecord -> IO ()
persistJobRecord cfg record = do
    let root = jobDirectory cfg (jobId record)
    createDirectoryIfMissing True (root </> "groups")
    createDirectoryIfMissing True (root </> "artifacts")
    BL.writeFile (root </> "job.json") (encode record)
    case jobResult record of
        Nothing -> pure ()
        Just result -> do
            BL.writeFile (root </> "artifacts" </> "manifest.json") (encode (resultArtifacts result))
            mapM_ (persistGroup root) (resultGroups result)

persistGroup :: FilePath -> DocumentGroup -> IO ()
persistGroup root groupDoc = do
    let groupRoot = root </> "groups" </> T.unpack (groupId groupDoc)
    createDirectoryIfMissing True groupRoot
    BL.writeFile
        (groupRoot </> "knowledge.txt")
        (BL.fromStrict (TE.encodeUtf8 (knowledgeText (groupKnowledge groupDoc))))
    BL.writeFile
        (groupRoot </> "knowledge.md")
        (BL.fromStrict (TE.encodeUtf8 (knowledgeMarkdown (groupKnowledge groupDoc))))
    BL.writeFile
        (groupRoot </> "metadata.json")
        (encode groupDoc)
    case knowledgeBenchmark (groupKnowledge groupDoc) of
        Nothing -> pure ()
        Just benchmarkSummary ->
            BL.writeFile
                (groupRoot </> "benchmark.json")
                (encode benchmarkSummary)

readArtifact :: AppConfig -> T.Text -> [T.Text] -> IO (Maybe BL.ByteString)
readArtifact cfg jobIdText artifactPath = do
    let path = foldl (</>) (jobDirectory cfg jobIdText) (map T.unpack artifactPath)
    exists <- doesFileExist path
    if exists then Just <$> BL.readFile path else pure Nothing

jobDirectory :: AppConfig -> T.Text -> FilePath
jobDirectory cfg jobIdText = storageRoot cfg </> T.unpack jobIdText
