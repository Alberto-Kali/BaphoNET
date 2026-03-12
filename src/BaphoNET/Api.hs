{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module BaphoNET.Api
    ( AppM
    , app
    , appServer
    , nt
    ) where

import BaphoNET.Domain
    ( AppConfig(..)
    , AppEnv(..)
    , AppState(..)
    , CreateJobRequest(..)
    , ArtifactManifest(..)
    , DocumentGroup(..)
    , JobRecord(..)
    , JobResult(..)
    , JobSummary(..)
    , JobStatus(..)
    , KnowledgeBundle(..)
    , emptySummary
    )
import BaphoNET.Pipeline (processRequest)
import BaphoNET.Storage (ensureStorageRoot, persistJobRecord, readArtifact)
import TransformController (ExtractionRequest, ExtractionResponse(..), extractContent)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
    ( atomically
    , modifyTVar'
    , readTVar
    , readTVarIO
    )
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.Wai (Application)
import Servant
    ( (:>), (:<|>)(..), Capture, CaptureAll, Get, Handler, JSON, OctetStream, Post, Proxy(..)
    , ReqBody, ServerT, err404, err400, err500, errBody, throwError, hoistServer, serve
    )

type AppM = ReaderT AppEnv Handler

type JobsApi =
       "hello" :> Get '[JSON] T.Text
  :<|> "transform" :> ReqBody '[JSON] ExtractionRequest :> Post '[JSON] JobResult
  :<|> "jobs" :> ReqBody '[JSON] CreateJobRequest :> Post '[JSON] JobRecord
  :<|> "jobs" :> Capture "jobId" T.Text :> Get '[JSON] JobRecord
  :<|> "jobs" :> Capture "jobId" T.Text :> "result" :> Get '[JSON] JobResult
  :<|> "jobs" :> Capture "jobId" T.Text :> "artifacts" :> CaptureAll "path" T.Text :> Get '[OctetStream] BL.ByteString

app :: AppEnv -> Application
app env = serve (Proxy :: Proxy JobsApi) (hoistServer (Proxy :: Proxy JobsApi) (nt env) appServer)

nt :: AppEnv -> AppM a -> Handler a
nt env action = runReaderT action env

appServer :: ServerT JobsApi AppM
appServer =
    helloHandler
        :<|> transformCompatHandler
        :<|> createJobHandler
        :<|> getJobHandler
        :<|> getResultHandler
        :<|> getArtifactHandler

helloHandler :: AppM T.Text
helloHandler = pure "BaphoNET v1"

transformCompatHandler :: ExtractionRequest -> AppM JobResult
transformCompatHandler request = do
    compatResponse <- liftIO (extractContent request)
    pure $
        JobResult
            { resultJobId = "compat-transform"
            , resultGroups =
                [ compatToGroup compatResponse ]
            , resultArtifacts = ArtifactManifest []
            }

createJobHandler :: CreateJobRequest -> AppM JobRecord
createJobHandler request = do
    env <- ask
    let cfg = config env
    if null (sources request)
        then throwError err400 {errBody = "sources must not be empty"}
        else
            if length (sources request) > maxJobSize cfg
                then throwError err400 {errBody = "sources exceed configured limit"}
                else do
                    liftIO (ensureStorageRoot cfg)
                    record <- liftIO (createQueuedJob env request)
                    _ <- liftIO (forkIO (runJob env (jobId record)))
                    pure record

getJobHandler :: T.Text -> AppM JobRecord
getJobHandler jobIdText = do
    env <- ask
    jobMap <- liftIO $ jobs <$> readTVarIO (state env)
    case Map.lookup jobIdText jobMap of
        Nothing -> throwError err404
        Just record -> pure record

getResultHandler :: T.Text -> AppM JobResult
getResultHandler jobIdText = do
    record <- getJobHandler jobIdText
    case jobResult record of
        Just result -> pure result
        Nothing ->
            case jobStatus record of
                JobFailed -> throwError err500 {errBody = "job failed"}
                _ -> throwError err404 {errBody = "result is not ready"}

getArtifactHandler :: T.Text -> [T.Text] -> AppM BL.ByteString
getArtifactHandler jobIdText artifactPath = do
    env <- ask
    bytes <- liftIO $ readArtifact (config env) jobIdText artifactPath
    case bytes of
        Nothing -> throwError err404
        Just payload -> pure payload

createQueuedJob :: AppEnv -> CreateJobRequest -> IO JobRecord
createQueuedJob env request = do
    now <- getCurrentTime
    atomically $ do
        appState <- readTVar (state env)
        let jobNumber = nextJobNumber appState
            newJobId = T.pack ("job-" <> show jobNumber)
            record =
                JobRecord
                    { jobId = newJobId
                    , jobStatus = JobQueued
                    , jobCreatedAt = now
                    , jobUpdatedAt = now
                    , jobRequest = request
                    , jobSummary = emptySummary
                    , jobResult = Nothing
                    , jobError = Nothing
                    }
        modifyTVar' (state env) $ \current ->
            current
                { jobs = Map.insert newJobId record (jobs current)
                , nextJobNumber = jobNumber + 1
                }
        pure record

runJob :: AppEnv -> T.Text -> IO ()
runJob env jobIdText = do
    _ <- updateJob env jobIdText (\record now -> record {jobStatus = JobRunning, jobUpdatedAt = now})
    appState <- readTVarIO (state env)
    case Map.lookup jobIdText (jobs appState) of
        Nothing -> pure ()
        Just record -> do
            resultE <- try @SomeException (processRequest (config env) jobIdText (jobRequest record))
            case resultE of
                Left ex ->
                    do
                        _ <- updateJob env jobIdText $ \job now ->
                            job
                                { jobStatus = JobFailed
                                , jobUpdatedAt = now
                                , jobError = Just (T.pack (show ex))
                                }
                        pure ()
                Right result -> do
                    updated <- updateJob env jobIdText $ \job now ->
                        job
                            { jobStatus = JobSucceeded
                            , jobUpdatedAt = now
                            , jobSummary =
                                JobSummary
                                    { summarySourceCount = length (sources (jobRequest job))
                                    , summaryGroupCount = length (resultGroups result)
                                    , summaryWarningCount =
                                        sum (map (length . knowledgeWarnings . groupKnowledge) (resultGroups result))
                                    }
                            , jobResult = Just result
                            , jobError = Nothing
                            }
                    maybe (pure ()) (persistJobRecord (config env)) updated

updateJob :: AppEnv -> T.Text -> (JobRecord -> UTCTime -> JobRecord) -> IO (Maybe JobRecord)
updateJob env jobIdText updater = do
    now <- getCurrentTime
    atomically $ do
        appState <- readTVar (state env)
        case Map.lookup jobIdText (jobs appState) of
            Nothing -> pure Nothing
            Just record -> do
                let updated = updater record now
                modifyTVar' (state env) $ \current ->
                    current {jobs = Map.insert jobIdText updated (jobs current)}
                pure (Just updated)

compatToGroup :: ExtractionResponse -> DocumentGroup
compatToGroup compatResponse =
    let bundle =
            KnowledgeBundle
                { knowledgeMarkdown = T.pack (extracted_text compatResponse)
                , knowledgeAbstract = T.take 280 (T.pack (extracted_text compatResponse))
                , knowledgeSourceIds = ["compat-source"]
                , knowledgeBacklinks = [T.pack (extracted_from compatResponse)]
                , knowledgeAssetRefs = []
                , knowledgeConfidence = 0.5
                , knowledgeWarnings = []
                }
    in DocumentGroup
        { groupId = "compat-group"
        , groupTitle = "Compatibility transform"
        , groupSummary = knowledgeAbstract bundle
        , groupSourceIds = ["compat-source"]
        , groupKnowledge = bundle
        }
