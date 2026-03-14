{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaphoNET.BrowserFetcher
    ( fetchUrlWithBrowser
    ) where

import BaphoNET.Domain
    ( BrowserFetchResult(..)
    , SeleniumConfig(..)
    , SourceDescriptor(..)
    )

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson
    ( FromJSON(..)
    , Result(..)
    , Value(..)
    , fromJSON
    , eitherDecode
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Simple
    ( Request
    , getResponseBody
    , httpLBS
    , parseRequest
    , setRequestBodyJSON
    , setRequestHeader
    , setRequestMethod
    , setRequestResponseTimeout
    )

data ResolvedImage = ResolvedImage
    { resolvedSrc :: T.Text
    , resolvedAlt :: Maybe T.Text
    } deriving (Eq, Show)

instance FromJSON ResolvedImage where
    parseJSON =
        withObject "ResolvedImage" $ \obj ->
            ResolvedImage
                <$> obj .: "src"
                <*> obj .:? "alt"

fetchUrlWithBrowser :: SeleniumConfig -> SourceDescriptor -> IO (Either T.Text BrowserFetchResult)
fetchUrlWithBrowser seleniumCfg src = do
    sessionE <- createSession seleniumCfg
    case sessionE of
        Left err -> pure (Left err)
        Right sessionIdText -> do
            navigateE <- navigateTo seleniumCfg sessionIdText (sourceValue src)
            result <-
                case navigateE of
                    Left err -> pure (Left err)
                    Right () -> do
                        waitForPage seleniumCfg
                        titleE <- fetchTitle seleniumCfg sessionIdText
                        sourceE <- fetchSource seleniumCfg sessionIdText
                        imagesE <- fetchResolvedImages seleniumCfg sessionIdText
                        pure $
                            case sourceE of
                                Left err -> Left err
                                Right htmlText ->
                                    Right
                                        BrowserFetchResult
                                            { fetchedHtml = htmlText
                                            , fetchedTitle = either (const Nothing) Just titleE
                                            , fetchedResolvedImages =
                                                either (const []) (map (\img -> (resolvedSrc img, resolvedAlt img))) imagesE
                                            , fetchedWarnings = collectWarnings titleE imagesE
                                            }
            _ <- destroySession seleniumCfg sessionIdText
            pure result

createSession :: SeleniumConfig -> IO (Either T.Text T.Text)
createSession seleniumCfg = do
    responseE <-
        sendJson seleniumCfg "POST" "/session" $
            object
                [ "capabilities" .=
                    object
                        [ "alwaysMatch" .=
                            object
                                [ "browserName" .= webdriverBrowserName (seleniumBrowser seleniumCfg)
                                , "goog:chromeOptions" .=
                                    object
                                        [ "args" .=
                                            [ "--headless=new" :: T.Text
                                            , "--disable-gpu"
                                            , "--no-sandbox"
                                            , "--disable-dev-shm-usage"
                                            ]
                                        ]
                                ]
                        ]
                ]
    case responseE of
        Left err -> pure (Left err)
        Right body ->
            case extractSessionId body of
                Just sessionIdText -> pure (Right sessionIdText)
                Nothing -> pure (Left "webdriver session id missing")

navigateTo :: SeleniumConfig -> T.Text -> T.Text -> IO (Either T.Text ())
navigateTo seleniumCfg sessionIdText urlText = do
    responseE <-
        sendJson seleniumCfg "POST" ("/session/" <> sessionIdText <> "/url") $
            object ["url" .= urlText]
    pure (const () <$> responseE)

fetchSource :: SeleniumConfig -> T.Text -> IO (Either T.Text T.Text)
fetchSource seleniumCfg sessionIdText = do
    responseE <-
        sendJson seleniumCfg "POST" ("/session/" <> sessionIdText <> "/execute/sync") $
            object
                [ "script" .=
                    ( "const root = document.querySelector('article') || document.querySelector('main') || document.body;"
                        <> "return root ? root.outerHTML : document.documentElement.outerHTML;"
                        :: T.Text
                    )
                , "args" .= ([] :: [Value])
                ]
    pure =<< decodeTextValue responseE

fetchTitle :: SeleniumConfig -> T.Text -> IO (Either T.Text T.Text)
fetchTitle seleniumCfg sessionIdText = do
    responseE <- sendRequest seleniumCfg "GET" ("/session/" <> sessionIdText <> "/title") Nothing
    pure =<< decodeTextValue responseE

fetchResolvedImages :: SeleniumConfig -> T.Text -> IO (Either T.Text [ResolvedImage])
fetchResolvedImages seleniumCfg sessionIdText = do
    responseE <-
        sendJson seleniumCfg "POST" ("/session/" <> sessionIdText <> "/execute/sync") $
            object
                [ "script" .=
                    ( "return Array.from(document.images)"
                        <> ".map(img => ({src: img.currentSrc || img.src || '', alt: img.alt || null}))"
                        <> ".filter(img => img.src && img.src.length > 0);"
                        :: T.Text
                    )
                , "args" .= ([] :: [Value])
                ]
    case responseE of
        Left err -> pure (Left err)
        Right body ->
            case extractArrayValue body of
                Right values -> pure (Right values)
                Left err -> pure (Left err)

destroySession :: SeleniumConfig -> T.Text -> IO (Either T.Text ())
destroySession seleniumCfg sessionIdText = do
    responseE <- sendRequest seleniumCfg "DELETE" ("/session/" <> sessionIdText) Nothing
    pure (const () <$> responseE)

waitForPage :: SeleniumConfig -> IO ()
waitForPage seleniumCfg =
    threadDelay (seleniumPageTimeoutSeconds seleniumCfg * 250000)

decodeTextValue :: Either T.Text BL.ByteString -> IO (Either T.Text T.Text)
decodeTextValue responseE =
    case responseE of
        Left err -> pure (Left err)
        Right body -> pure (extractTextValue body)

collectWarnings :: Either T.Text a -> Either T.Text b -> [T.Text]
collectWarnings titleE imagesE =
    [ "webdriver title lookup failed: " <> err | Left err <- [titleE] ]
        <> [ "webdriver image lookup failed: " <> err | Left err <- [imagesE] ]

sendJson :: SeleniumConfig -> T.Text -> T.Text -> Value -> IO (Either T.Text BL.ByteString)
sendJson seleniumCfg method path payload =
    sendRequest seleniumCfg method path (Just payload)

sendRequest :: SeleniumConfig -> T.Text -> T.Text -> Maybe Value -> IO (Either T.Text BL.ByteString)
sendRequest seleniumCfg method path mPayload = do
    requestE <- try @SomeException (buildRequest seleniumCfg method path mPayload)
    case requestE of
        Left ex -> pure (Left (T.pack (show ex)))
        Right request -> do
            responseE <- try @SomeException (httpLBS request)
            case responseE of
                Left ex -> pure (Left (T.pack (show ex)))
                Right response -> pure (Right (getResponseBody response))

buildRequest :: SeleniumConfig -> T.Text -> T.Text -> Maybe Value -> IO Request
buildRequest seleniumCfg method path mPayload = do
    request <- parseRequest (seleniumDriverUrl seleniumCfg <> T.unpack path)
    let baseRequest =
            setRequestMethod (encodeText method)
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestResponseTimeout (responseTimeoutMicro (seleniumPageTimeoutSeconds seleniumCfg * 1000000))
                $ request
    pure $
        case mPayload of
            Nothing -> baseRequest
            Just payload -> setRequestBodyJSON payload baseRequest

encodeText :: T.Text -> BS.ByteString
encodeText = TE.encodeUtf8

webdriverBrowserName :: T.Text -> T.Text
webdriverBrowserName browserName
    | T.toLower browserName == "chromium" = "chrome"
    | otherwise = browserName

extractSessionId :: BL.ByteString -> Maybe T.Text
extractSessionId body =
    case eitherDecode body of
        Left _ -> Nothing
        Right value ->
            case value of
                Object obj ->
                    case lookupKey "value" obj of
                        Just innerValue -> objectSessionId innerValue <|> objectSessionId value
                        Nothing -> objectSessionId value
                _ -> Nothing
  where
    objectSessionId value =
        case value of
            Object obj -> lookupKey "sessionId" obj >>= textValue
            _ -> Nothing
    lookupKey key obj = case obj KeyMap.!? Key.fromText key of
        Just v -> Just v
        Nothing -> Nothing
    textValue (String txt) = Just txt
    textValue _ = Nothing

extractTextValue :: BL.ByteString -> Either T.Text T.Text
extractTextValue body =
    case eitherDecode body of
        Left decodeErr -> Left (T.pack decodeErr)
        Right value ->
            case valueField value of
                Just (String txt) -> Right txt
                _ -> Left "webdriver text payload missing"

extractArrayValue :: BL.ByteString -> Either T.Text [ResolvedImage]
extractArrayValue body =
    case eitherDecode body of
        Left decodeErr -> Left (T.pack decodeErr)
        Right value ->
            case valueField value of
                Just arrayValue ->
                    case fromJSON arrayValue of
                        Success images -> Right images
                        Error err -> Left (T.pack err)
                Nothing -> Left "webdriver image payload missing"

valueField :: Value -> Maybe Value
valueField (Object obj) = lookupObjectKey "value" obj
valueField _ = Nothing

lookupObjectKey :: T.Text -> KeyMap.KeyMap Value -> Maybe Value
lookupObjectKey key obj = obj KeyMap.!? Key.fromText key
