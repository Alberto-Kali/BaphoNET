{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module TransformController
    ( extractContent
    , ExtractionRequest(..)
    , ExtractionResponse(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody)
import System.Directory (doesFileExist)
import Text.Pandoc (readHtml, readMarkdown, writePlain, def, PandocError, runIO)
import Control.Exception (try, SomeException)

data ExtractionRequest = ExtractionRequest 
    { data_type :: String
    , link :: String
    , file :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data ExtractionResponse = ExtractionResponse
    { extracted_text :: String
    , extracted_from :: String
    , extraction_time :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

extractContent :: ExtractionRequest -> IO ExtractionResponse
extractContent req = do
    startTime <- getCurrentTime
    let url = link req
        filePath = file req
        dataType = data_type req
        source = if not (null url) then url else filePath

    -- Явно указываем, что хотим ловить любые исключения (SomeException)
    contentE <- try @SomeException $ if not (null filePath)
                                      then readLocalFile filePath
                                      else fetchUrl url

    case contentE of
        Left ex -> do
            endTime <- getCurrentTime
            let elapsed = round (diffUTCTime endTime startTime)
            return $ ExtractionResponse ("Error fetching content: " ++ show ex) source elapsed
        Right content -> do
            convResult <- case dataType of
                "html" -> convertHtmlToPlain content
                "md"   -> convertMarkdownToPlain content
                _      -> convertHtmlToPlain content
            endTime <- getCurrentTime
            let elapsed = round (diffUTCTime endTime startTime)
            case convResult of
                Left err -> return $ ExtractionResponse ("Conversion error: " ++ show err) source elapsed
                Right text -> return $ ExtractionResponse (T.unpack text) source elapsed

readLocalFile :: FilePath -> IO T.Text
readLocalFile path = do
    exists <- doesFileExist path
    if exists
        then TIO.readFile path
        else return $ T.pack $ "File not found: " ++ path

fetchUrl :: String -> IO T.Text
fetchUrl urlStr = do
    request <- parseRequest urlStr
    response <- httpLBS request
    let body = getResponseBody response
    return $ TE.decodeUtf8 $ BL.toStrict body

convertHtmlToPlain :: T.Text -> IO (Either PandocError T.Text)
convertHtmlToPlain html = runIO $ do
    doc <- readHtml def html
    writePlain def doc

convertMarkdownToPlain :: T.Text -> IO (Either PandocError T.Text)
convertMarkdownToPlain md = runIO $ do
    doc <- readMarkdown def md
    writePlain def doc